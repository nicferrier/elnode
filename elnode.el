;;; elnode.el --- a simple emacs async HTTP server -*- lexical-binding: t -*-

;; Copyright (C) 2010  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 5th October 2010
;; Version: 0.2
;; Keywords: lisp, http

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an elisp version of the popular node.js asynchronous
;; webserver toolkit.
;;
;; You can define HTTP request handlers and start an HTTP server
;; attached to the handler.  Many HTTP servers can be started, each
;; must have it's own TCP port.  Handlers can defer processing with a
;; signal (which allows comet style resource management)
;;
;; See elnode-start for how to start an HTTP server.

;;; Source code
;;
;; elnode's code can be found here:
;;   http://github.com/nicferrier/elnode

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elnode--private-function
;;
;; for private functions.


;;; Code:

(require 'mm-encode)
(require 'mailcap)

(eval-when-compile (require 'cl))

(defvar elnode-server-socket nil
  "Where we store the server sockets.

This is an alist of proc->server-process:

  (port . process)")

(defvar elnode-server-error-log "*elnode-server-error*"
  "The buffer where error log messages are sent.")

;; Error log handling

(defun elnode-error (msg &rest args)
  "Log MSG with ARGS as an error.

This function is available for handlers to call.  It is also used
by elnode iteslf.

There is only one error log, in the future there may be more."
  (with-current-buffer (get-buffer-create elnode-server-error-log)
    (goto-char (point-max))
    (insert (format "elnode-%s: %s\n"
		    (format-time-string "%Y%m%d%H%M%S")
		    (if (car-safe args)
			(apply 'format `(,msg ,@args))
		      msg)))))


;; Defer stuff

(put 'elnode-defer 'error-conditions '(elnode-defer)) ;; the elnode defer signal

(defvar elnode--deferred
  '()
  "list of deferred pairs: (socket . handler)")

(defun elnode-defer-now (handler)
  "The function you call to defer processing of the current socket.

Pass in the current handler.

FIXME: We could capture the current handler somehow? I think the
point is that whatever signals elnode-defer should be getting
control back when the deferred is re-processed."
  (signal 'elnode-defer handler)
  )

(defmacro elnode-defer-or-do (guard &rest body)
  "Test the GUARD and defer if it suceeds and BODY if it doesn't."
  `(if ,guard
       (elnode-defer-now)
     (progn
       ,@body))
  )

(defun elnode--deferred-add (httpcon handler)
  "Add the specified HTTPCON HANDLER pair to the list to be processed later.

Basically, add the HTTPCON connection and the HANDLER that is
dealing with it to enable comet like behaviour."
  ;; Update the elnode--deferred list directly.
  ;; Remember, there are no concurrency issues here.
  (if elnode--deferred
      (setcdr
       (last elnode--deferred)
       (cons `(,httpcon . ,handler) nil))
    (setq elnode--deferred (cons `(,httpcon . ,handler) nil))
    )
  )

(defun elnode--deferred-processor ()
  "Called by an idle timer to process any deferred socket/handler pairs.

It's this that gives elnode the ability to be a COMET server."
  (let* ((lst 'elnode--deferred))
    (while (eval lst)
      (let* ((pair (car (eval lst)))
             (httpcon (car pair))
             (handler (cdr pair)))
        ;; This could benefit from a try/catch/else type form
        (catch 'next
          (condition-case signal-value
              ;; Defer handling - for comet style operations
              (funcall handler httpcon)
            ('elnode-defer
             ;; We need to continue to defer this
             ;; basically it means not removing it from the list
             (throw 'next 't)))
          ;; We completed without a defer signal so we need to remove the pair
          (set lst (cdr (eval lst)))
          )
        )
      )
    )
  )

(defvar elnode--defer-timer nil
  "The timer used by the elnode defer processing.

This is initialized by `elnode--init-deferring'."
  )

(defun elnode--init-deferring ()
  "Initialize elnode defer processing.  Necessary for running comet apps."
  (setq elnode--defer-timer
        (run-with-idle-timer 0.1 't 'elnode--deferred-processor))
  )


;; Main control functions

(defun elnode--sentinel (process status)
  "Sentinel function for the main server and for the client sockets."
  (cond
   ;; Server status
   ((and
     (assoc (process-contact process :service) elnode-server-socket)
     (equal status "deleted\n"))
    (kill-buffer (process-buffer process))
    (elnode-error "Elnode server stopped"))

   ;; Client socket status
   ((equal status "connection broken by remote peer\n")
    (if (process-buffer process)
	(progn
	  (kill-buffer (process-buffer process))
	  (elnode-error "Elnode connection dropped")))
    )

   ((equal status "open\n") ;; this says "open from ..."
    (elnode-error "Elnode opened new connection"))

   ;; Default
   (t
    (elnode-error "Elnode status: %s %s" process status))
   ))

(defun elnode--filter (process data)
  "Filter for the clients.

This does the work of finding and calling the user http
connection handler for a request.

A buffer for the http connection is created, uniquified by the
port number of the connection."
  (let ((buf (or
              (process-buffer process)
              ;; Set the process buffer (because the server doesn't automatically allocate them)
              ;; the name of the buffer has the client port in it
              ;; the space in the name ensures that emacs does not list it
              (let* ((port (cadr (process-contact process))))
                (set-process-buffer
                 process
                 (get-buffer-create (format " *elnode-request-%s*" port)))
                (process-buffer process)))))
    (with-current-buffer buf
      (insert data)
      ;; We need to check the buffer for \r\n\r\n which marks the end of HTTP header
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "\r\n\r\n" nil 't)
            (let ((server (process-get process :server)))
              ;; This is where we call the user handler
              ;; TODO: this needs error protection so we can return an error?
              (condition-case signal-value
                  ;; Defer handling - for comet style operations
                  (funcall (process-get server :elnode-http-handler) process)
                ('elnode-defer
                 ;; The handler's processing of the socket should be deferred
                 ;; - the value of the signal is the current handler (see elnode-defer-now)
                 (elnode--deferred-add process (cdr signal-value)))
                ('t
                 ;; Try and send a 500 error response
                 ;; FIXME: we need some sort of check to see if the header has been written
                 (process-send-string
                  process
                  "HTTP/1.1 500 Server-Error\r\n<h1>Server Error</h1>\r\n")))))))))


(defun elnode--log-fn (server con msg)
  "Log function for elnode.

Serves only to connect the server process to the client processes"
  (process-put con :server server)
  )

(defvar elnode-handler-history '()
  "The history of handlers bound to servers.")

(defvar elnode-port-history '()
  "The history of ports that servers are started on.")

(defvar elnode-host-history '()
  "The history of hosts that servers are started on.")

;;;###autoload
(defun elnode-start (request-handler port host)
  "Start the elnode server so that REQUEST-HANDLER handles requests on PORT on HOST.

Most of the work done by the server is actually done by
functions, the sentinel function, the log function and a filter
function.

request-handler is a function which is called with the
request. The function is called with one argument, the
http-connection.

You can use functions such as elnode-http-start and
elnode-http-send-body to send the http response.

Example:

 (defun nic-server (httpcon)
   (elnode-http-start 200 '((\"Content-Type\": \"text/html\")))
   (elnode-http-return \"<html><b>BIG!</b></html>\")
   )
 (elnode-start 'nic-server 8000)
 ;; End

You must also specify the port to start the server on.

You can optionally specify the hostname to start the server on,
this must be bound to a local IP. Some names are special:

  localhost  means 127.0.0.1
  * means 0.0.0.0

specifying an IP is also possible.

Note that although host can be specified, elnode does not
disambiguate on running servers by host. So you cannot start 2
different elnode servers on the same port on different hosts."
  (interactive
   (let ((handler (completing-read "Handler function: "
                                   obarray 'fboundp t nil nil))
         (port (read-number "Port: " nil))
         (host (read-string "Host: " "localhost" 'elnode-host-history)))
     (list (intern handler) port host)))
  (if (not (assoc port elnode-server-socket))
      ;; Add a new server socket to the list
      (setq elnode-server-socket
            (cons
             (cons port
                   (let ((buf (get-buffer-create "*elnode-webserver*")))
                     (make-network-process
                      :name "*elnode-webserver-proc*"
                      :buffer buf
                      :server t
                      :nowait 't
                      :host (cond
                             ((equal host "localhost")
                              'local)
                             ((equal host "*")
                              nil)
                             (t
                              host))
                      :service port
                      :coding '(raw-text-unix . raw-text-unix)
                      :family 'ipv4
                      :filter 'elnode--filter
                      :sentinel 'elnode--sentinel
                      :log 'elnode--log-fn
                      :plist `(:elnode-http-handler ,request-handler))))
             elnode-server-socket))))

;; TODO: make this take an argument for the
(defun elnode-stop (port)
  "Stop the elnode server attached to PORT."
  (interactive "nPort: ")
  (let ((server (assoc port elnode-server-socket)))
    (if server
        (progn
          (delete-process (cdr server))
          (setq elnode-server-socket
		;; remove-if
		(let ((test (lambda (elem)
			      (= (car elem) port)))
		      (l elnode-server-socket)
		      result)
		  (while (car l)
		    (let ((p (pop l))
			  (r (cdr l)))
		      (if (not (funcall test p))
			  (setq result (cons p result)))))
		  result))))))

(defun elnode-list-buffers ()
  "List the current buffers being managed by elnode."
  (interactive)
  (with-current-buffer (get-buffer-create "*elnode-buffers*")
    (erase-buffer)
    (mapc
     (lambda (b)
       (save-excursion
         (if (string-match " \\*elnode-.*" (buffer-name b))
             (insert (format "%s\n" b)))
       ))
     (sort (buffer-list)
           (lambda (a b)
             (string-lessp (buffer-name b) (buffer-name a))))))
  (display-buffer (get-buffer "*elnode-buffers*")))

;; HTTP API methods

(defun elnode--http-parse (httpcon)
  "Parse the HTTP header for the process.

Returns a cons of the status line and the header association-list:

 (http-status . http-header-alist)"
  (with-current-buffer (process-buffer httpcon)
    (save-excursion
      (goto-char (point-min))
      (let ((hdrend (re-search-forward "\r\n\r\n" nil 't)))
        ;; It's an error if we can't find the end of header because
        ;; elnode--filter should not have called the user handler
        ;; until the header has ended
        (if (not hdrend)
            (error "Elnode: the header was not found by the HTTP parsing routines"))
        ;; Split the lines from the beginning of the buffer to the
        ;; header end, use the first as the status line and the rest as the header
        ;; FIXME: we don't handle continuation lines of anything like that
        (let* ((lines (split-string (buffer-substring (point-min) hdrend) "\r\n" 't))
               (status (car lines))
               (header (cdr lines)))
          (process-put httpcon :elnode-header-end hdrend)
          (process-put httpcon :elnode-http-status status)
          (process-put
           httpcon
           :elnode-http-header
           (mapcar
            (lambda (hdrline)
              (if (string-match "\\([A-Za-z0-9_-]+\\): \\(.*\\)" hdrline)
                  (cons (match-string 1 hdrline) (match-string 2 hdrline))))
            header))))
      (cons
       (process-get httpcon :elnode-http-status)
       (process-get httpcon :elnode-http-header)))))

(defun elnode-http-header (httpcon name)
  "Get the header specified by name from the header."
  (let ((hdr (or
              (process-get httpcon :elnode-http-header)
              (cdr (elnode--http-parse httpcon)))))
    (cdr (assoc name hdr))))

(defun elnode--http-parse-status (httpcon &optional property)
  "Parse the status line.

property if specified is the property to return"
  (let ((http-line (or
                    (process-get httpcon :elnode-http-status)
                    (car (elnode--http-parse httpcon)))))
    (string-match
     "\\(GET\\|POST\\|HEAD\\) \\(.*\\) HTTP/\\(1.[01]\\)"
     http-line)
    (process-put httpcon :elnode-http-method (match-string 1 http-line))
    (process-put httpcon :elnode-http-resource (match-string 2 http-line))
    (process-put httpcon :elnode-http-version (match-string 3 http-line))
    (if property
        (process-get httpcon property))))

(defun elnode--http-parse-resource (httpcon &optional property)
  "Convert the specified resource to a path and a query"
  (save-match-data
    (let ((resource
           (or
            (process-get httpcon :elnode-http-resource)
            (elnode--http-parse-status httpcon :elnode-http-resource))))
      (or
       ;; root pattern
       (string-match "^\\(/\\)\\(\\?.*\\)*$" resource)
       ;; /somepath or /somepath/somepath
       (string-match "^\\(/[A-Za-z0-9_/.-]+\\)\\(\\?.*\\)*$" resource))
      (let ((path (match-string 1 resource)))
        (process-put httpcon :elnode-http-pathinfo path))
      (if (match-string 2 resource)
          (let ((query (match-string 2 resource)))
            (string-match "\\?\\(.+\\)" query)
            (if (match-string 1 query)
                (process-put httpcon :elnode-http-query (match-string 1 query)))))))
  (if property
      (process-get httpcon property)))

(defun elnode-http-pathinfo (httpcon)
  "Get the PATHINFO of the request."
  (or
   (process-get httpcon :elnode-http-pathinfo)
   (elnode--http-parse-resource httpcon :elnode-http-pathinfo)))

(defun elnode-http-query (httpcon)
  "Get the QUERY of the request."
  (or
   (process-get httpcon :elnode-http-query)
   (elnode--http-parse-resource httpcon :elnode-http-query)))

(defun elnode--http-query-to-alist (query)
  "Crap parser for HTTP QUERY data.
Returns an association list."
  (let ((alist (mapcar
                (lambda (nv)
                  (string-match "\\([^=]+\\)\\(=\\(.*\\)\\)*" nv)
                  (cons
                   (match-string 1 nv)
                   (if (match-string 2 nv)
                       (match-string 3 nv)
                     nil)))
                (split-string query "&"))
               ))
    alist))

(defun elnode--alist-merge (a b &optional operator)
  "Merge two association lists non-destructively.

A is considered the priority (it's elements go in first)."
  (if (not operator)
      (setq operator 'assq))
  (let* ((res '()))
    (let ((lst (append a b)))
      (while lst
        (let ((item (car-safe lst)))
          (setq lst (cdr-safe lst))
          (let* ((key (car item))
                 (aval (funcall operator key a))
                 (bval (funcall operator key b)))
            (if (not (funcall operator key res))
                (setq res (cons
                           (if (and aval bval)
                               ;; the item is in both lists
                               (cons (car item)
                                     (list (cdr aval) (cdr bval)))
                             item)
                           res))))))
        res)))

(defun elnode--http-post-to-alist (httpcon)
  "Parse the POST body.
This is not a strong parser. Replace with something better."
  (let ((postdata
         (with-current-buffer (process-buffer httpcon)
           (buffer-substring
            ;; we might have to add 2 to this because of trailing \r\n
            (process-get httpcon :elnode-header-end)
            (point-max)))))
    (elnode--http-query-to-alist postdata)))

(defun elnode-http-params (httpcon)
  "Get an alist of the parameters in the request.

If the method is a GET then the parameters are from the url. If
the method is a POST then the parameters may come from either the
url or the POST body or both:

 POST /path?a=b&x=y
 a=c

would result in:

 '(('a' 'b' 'c')('x' . 'y'))"
  (or
   (process-get httpcon :elnode-http-params)
   (let ((query (elnode-http-query httpcon)))
     (let ((alist (if query
                      (elnode--http-query-to-alist query)
                    '())))
       (if (equal "POST" (elnode-http-method httpcon))
           (progn
             (setq alist (elnode--alist-merge
                          alist
                          (elnode--http-post-to-alist httpcon)
                          'assoc))
             (process-put httpcon :elnode-http-params alist)
             alist)
         ;; Else just return nil
         '())))))

(defun elnode-http-param (httpcon name)
  "Get the named parameter from the request."
  (let* ((params (elnode-http-params httpcon))
         (param-pair (assoc name params)))
    (if param-pair
        (cdr param-pair))
    ;; Should we signal when we don't have a param?
    ))

(defun elnode-http-method (httpcon)
  "Get the PATHINFO of the request."
  (or
   (process-get httpcon :elnode-http-method)
   (elnode--http-parse-status httpcon :elnode-http-method)))

(defun elnode-http-version (httpcon)
  "Get the PATHINFO of the request."
  (or
   (process-get httpcon :elnode-http-version)
   (elnode--http-parse-status httpcon :elnode-http-version)))

(defun elnode-http-send-string (httpcon str)
  "Send the string to the HTTP connection.

This is really only a placeholder function for doing transfer-encoding."
  ;; We should check that we are actually doing chunked encoding...
  ;; ... but for now we just presume we're doing it.
  (let ((len (length str)))
    (process-send-string httpcon (format "%x\r\n%s\r\n" len (or str "")))
    )
  )

(defun elnode-http-start (httpcon status &rest header)
  "Start the http response on the specified http connection.

httpcon is the HTTP connection being handled.
status is the HTTP status, eg: 200 or 404
header is a sequence of (header-name . value) pairs.

For example:

 (elnode-http-start httpcon \"200\" '(\"Content-type\" . \"text/html\"))
"
  (if (process-get httpcon :elnode-http-started)
      (elnode-error "Http already started")
    (let ((http-codes-strings '(("200" . "Ok")           (200 . "Ok")
                                ("302" . "Redirect")     (302 . "Redirect")
                                ("400" . "Bad Request")  (400 . "Bad Request")
                                ("401" . "Authenticate") (401 . "Authenticate")
                                ("404" . "Not Found")    (404 . "Not Found")
                                ("500" . "Server Error") (500 . "Server Error")
                                )))
      ;; Send the header
      (let ((header-alist (cons '("Transfer-encoding" . "chunked") header)))
        (process-send-string
         httpcon
         (format
          "HTTP/1.1 %s %s\r\n%s\r\n\r\n"
          status
          ;; The status text
          (cdr (assoc status http-codes-strings))
          ;; The header
          (or
           (mapconcat
            (lambda (p)
              (format "%s: %s" (car p) (cdr p)))
            header-alist
            "\r\n")
           "\r\n")))
        (process-put httpcon :elnode-http-started 't)))))

(defun elnode--http-end (httpcon)
  "We need a special end function to do the emacs clear up."
  (process-send-eof httpcon)
  (delete-process httpcon)
  (kill-buffer (process-buffer httpcon))
  )

(defun elnode-http-return (httpcon &optional data)
  "End the http response on the specified 'httpcon' optionally sending 'data' first.

'httpcon' is the http connection which must have had the headers
sent with 'elnode-http-start'

'data' must be a string, it's just passed to 'elnode-http-send'."
  (if (not (process-get httpcon :elnode-http-started))
      (elnode-error "Http not started")
    (progn
      (if data
          (elnode-http-send-string httpcon data))
      ;; Need to close the chunked encoding here
      (elnode-http-send-string httpcon "")
      (process-send-string httpcon "\r\n")
      (elnode--http-end httpcon))))


(defun elnode--mapper-find (path url-mapping-table)
  "Try and find the 'PATH' inside the 'URL-MAPPING-TABLE'.

This function exposes it's `match-data' on the 'path' variable so
that you can access that in your handler with something like:

 (match-string 1 (elnode-http-pathinfo httpcon))"
  (elnode-error "Elnode--mapper-find path: %s" path)
  ;; Implement a simple escaping find function
  (catch 'found
    (mapcar
     (lambda (mapping)
       (let ((mapping-re (format "^/%s" (car mapping))))
         (if (string-match mapping-re path)
             (throw 'found mapping))))
     url-mapping-table)))


(defun elnode-send-404 (httpcon)
  "A generic 404 handler."
  (elnode-http-start httpcon 404 '("Content-type" . "text/html"))
  (elnode-http-return httpcon "<h1>Not Found</h1>\r\n"))

(defun elnode-send-400 (httpcon)
  "A generic 400 handler."
  (elnode-http-start httpcon 400 '("Content-type" . "text/html"))
  (elnode-http-return httpcon "<h1>Bad request</h1>\r\n"))

(defun elnode-send-redirect (httpcon location)
  "Sends a redirect to the specified location."
  (elnode-http-start httpcon 302 `("Location" . ,location))
  (elnode-http-return httpcon (format "<h1>redirecting you to %s</h1>\r\n" location)))

(defun elnode-normalize-path (httpcon handler)
  "A decorator for 'handler' that normalizes paths to have a trailing slash.

This checks the path for a trailing slash and sends a 302 to the
slash trailed url if there is none.

Otherwise it calls 'handler'"
  (if (not (save-match-data
             (string-match ".*\\(/\\|.*\\.[^/]*\\)$" (elnode-http-pathinfo httpcon))))
      (elnode-send-redirect httpcon (format "%s/" (elnode-http-pathinfo httpcon)))
    (funcall handler httpcon)))


(defun elnode--dispatch-proc (httpcon url-mapping-table &optional function-404)
  "Does the actual dispatch work."
  (let* ((pi (elnode-http-pathinfo httpcon))
         (m (elnode--mapper-find pi url-mapping-table)))
    (if (and m (functionp (cdr m)))
        (funcall (cdr m) httpcon)
      ;; We didn't match so fire a 404... possibly a custom 404
      (if (functionp function-404)
          (funcall function-404 httpcon)
        ;; We don't have a custom 404 so send our own
        (elnode-send-404 httpcon)))))

(defun elnode-dispatcher (httpcon url-mapping-table &optional function-404)
  "Dispatch the HTTPCON to the correct function based on the URL-MAPPING-TABLE.

URL-MAPPING-TABLE is an alist of:

 (url-regex . function-to-dispatch)

To map the root url you should use:

  $

'elnode-dispatcher' uses 'elnode-normalize-path' to ensure paths
end in / so to map another url you should use:

  path/$

or:

  path/subpath/$"
  (elnode-normalize-path
   httpcon
   (lambda (httpcon)
     (elnode--dispatch-proc httpcon url-mapping-table function-404))))

(defun elnode--hostpath-dispatch-proc (httpcon hostpath-mapping-table &optional function-404)
  "Does the actual hostpath dispatch work."
  ;; TODO - find a way to abstract this and elnode--dispatch-proc
  (let* ((hostpath (format "%s%s"
                    (let ((host (elnode-http-header httpcon "Host")))
                      (save-match-data
                        (string-match "\\([^:]+\\)\\(:[0-9]+.*\\)" host)
                        (match-string 1 host)))
                    (elnode-http-pathinfo httpcon)))
         (m (catch 'found
             (mapcar
              (lambda (mapping)
                (let ((mapping-re (car mapping)))
                  (if (string-match mapping-re hostpath)
                      (throw 'found mapping))))
              hostpath-mapping-table))))
    (if (and m (functionp (cdr m)))
        (funcall (cdr m) httpcon)
      ;; We didn't match so fire a 404... possibly a custom 404
      (if (functionp function-404)
          (funcall function-404 httpcon)
        ;; We don't have a custom 404 so send our own
        (elnode-send-404 httpcon)))))

(defun elnode-hostpath-dispatcher (httpcon hostpath-mapping-table &optional function-404)
  "Dispatch the HTTPCON to the correct handler based on the HOSTPATH-MAPPING-TABLE.

HOSTPATH-MAPPING-TABLE has a regex of the host and the path slash separated, thus:

 (\"^localhost/pastebin.*\" . pastebin-handler)"
  (elnode-normalize-path
   httpcon
   (lambda (httpcon)
     (elnode--hostpath-dispatch-proc httpcon hostpath-mapping-table function-404))))

;;;###autoload
(defcustom elnode-hostpath-default-table
  '(("[^/]+/.*" . elnode-webserver))
  "Customizable variable defining hostpath mappings for 'elnode-hostpath-default-handler'.

This is the default mapping table for elnode, out of the box. If
you customize this then elnode will serve these hostpath mappings
by just loading elnode.

By default the table maps everything to
'elnode-webserver'. Unless you're happy with the default you
should probably get rid of the everything path because it will
interfere with any other mappings you add."
  :group 'elnode)

;;;###autoload
(defun elnode-hostpath-default-handler (httpcon)
  "A hostpath handler using the 'elnode-hostpath-default-table' for the match table.

This simply calls 'elnode-hostpath-dispatcher' with 'elnode-hostpath-default-table'."
  (elnode-hostpath-dispatcher httpcon elnode-hostpath-default-table))


;; elnode child process functions

;; TODO: handle errors better than messaging
(defun elnode--child-process-sentinel (process status)
  "A generic sentinel for elnode child processes.

elnode child processes are just emacs asynchronous processes that
send their output to an elnode http connection.

The main job of this sentinel is to send the end of the http
stream when the child process finishes."
  (cond
   ((equal status "finished\n")
    (let ((httpcon (process-get process :elnode-httpcon)))
      (elnode-error "Status @ finished: %s -> %s" (process-status httpcon) (process-status process))
      (if (not (eq 'closed (process-status httpcon)))
	  (progn
	    (elnode-http-send-string httpcon  "")
	    (process-send-string httpcon "\r\n")
	    (elnode--http-end httpcon)))))
   ((string-match "exited abnormally with code \\([0-9]+\\)\n" status)
    (let ((httpcon (process-get process :elnode-httpcon)))
      (if (not (eq 'closed (process-status httpcon)))
	  (progn
	    (elnode-http-send-string httpcon "")
	    (process-send-string httpcon "\r\n")
	    (elnode--http-end httpcon)))
      (delete-process process)
      (kill-buffer (process-buffer process))
      (elnode-error "Elnode-child-process-sentinel: %s" status)))
   (t
    (elnode-error "Elnode-chlild-process-sentinel: %s" status))))

(defun elnode--child-process-filter (process data)
  "A generic filter function for elnode child processes.

elnode child processes are just emacs asynchronous processes that
send their output to an elnode http connection.

This filter function does the job of taking the output from the
async process and finding the associated elnode http connection
and sending the data there."
  (let ((httpcon (process-get process :elnode-httpcon)))
    (elnode-error "Elnode-child-process-filter http state: %s data length: %s"
		  (process-status httpcon)
		  (length data)
		  )
    (if (not (equal "closed" (process-status httpcon)))
	(elnode-http-send-string httpcon data))))

(defun elnode-child-process (httpcon program &rest args)
  "Run the specified process asynchronously and send it's output to the http connection.

program is the program to run.
args is a list of arguments to pass to the program.

It is NOT POSSIBLE to run more than one process at a time
directed at the same http connection."
  (let* ((args `(,(format "%s-%s" (process-name httpcon) program)
                 ,(format " %s-%s" (process-name httpcon) program)
                 ,program
                 ,@args
                ))
         (p (let ((process-connection-type nil))
	      (apply 'start-process args))))
    (set-process-coding-system p 'raw-text-unix)
    ;; Bind the http connection to the process
    (process-put p :elnode-httpcon httpcon)
    ;; Bind the process to the http connection
    ;; WARNING: this means you can only have 1 child process at a time
    (process-put httpcon :elnode-child-process p)
    ;; Setup the filter and the sentinel to do the right thing with incomming data and signals
    (set-process-filter p 'elnode--child-process-filter)
    (set-process-sentinel p 'elnode--child-process-sentinel)))

;; Webserver stuff

(defcustom elnode-webserver-docroot "~/public_html"
  "The document root of the webserver.

Webserver functions are free to use this or not.  The
'elnode-webserver' function does use it."
  :group 'elnode)

(defcustom elnode-webserver-extra-mimetypes '(("text/plain" . "creole")
                                               ("text/plain" . "el"))
  "this is just a way of hacking the mime type discovery so we
can add more file mappings more easily than editing
/etc/mime.types"
  :group 'elnode)


(defun elnode--webserver-index (docroot targetfile pathinfo)
  "Constructs index documents for a 'DOCROOT' and 'TARGETFILE' pointing to a dir."
  ;; TODO make this usable by people generally
  (let ((dirlist (directory-files-and-attributes targetfile)))
    ;; TODO make some templating here so people can change this
    (format
     "<html><head><title>%s</title></head><body><h1>%s</h1><div>%s</div></body></html>\n"
     pathinfo
     pathinfo
     (mapconcat
      (lambda (dir-entry)
        (let ((entry (format
                      "%s%s"
                      (if (equal pathinfo "/")  "" pathinfo)
                      (car dir-entry))))
          (format
           "<a href='%s'>%s</a><br/>\r\n"
           entry
           (car dir-entry))))
      dirlist
      "\n"))))

(defun elnode-test-path (httpcon docroot handler &optional 404-handler)
  "Check that the path requested is above the docroot specified.

Call 404-handler (or default 404 handler) on failure and handler
on success.

handler is called: httpcon docroot targetfile

This is used by 'elnode--webserver-handler-proc' in the webservers
that it creates... but it's also meant to be generally useful for
other handler writers."
  (let* ((pathinfo (elnode-http-pathinfo httpcon))
         ;; Let webserver users prefix the webserver path in a dispatcher regex
         ;; use a regex like this:
         ;;  "prefix/\\(.*\\)$"
         ;; and we'll be able to prefix the path properl
         (path (or (match-string 1 pathinfo) pathinfo))
         (targetfile (format "%s%s"
                             (expand-file-name docroot)
                             (format "/%s" (if (equal path "/")  "" path)))))
    (if (or
         (file-exists-p targetfile)
         ;; Test the targetfile is under the docroot
         (let ((docrootlen (length docroot)))
           (compare-strings
            docroot 0 docrootlen
            (file-truename targetfile) 0 docrootlen)))
        (funcall handler httpcon docroot targetfile)
      ;; Call the 404 handler
      (if (functionp 404-handler)
          (funcall 404-handler httpcon)
        (elnode-send-404 httpcon)))))


(defun elnode--webserver-handler-proc (httpcon docroot mime-types)
  "Actual webserver implementation.

This is not a real handler (because it takes more than the
httpcon) but it is called directly by the real webserver
handlers."
  (elnode-test-path
   httpcon docroot
   (lambda (httpcon docroot targetfile)
     ;; The file exists and is legal
     (let ((pathinfo (elnode-http-pathinfo httpcon)))
       (if (file-directory-p targetfile)
           (let ((index (elnode--webserver-index docroot targetfile pathinfo)))
             ;; What's the best way to do simple directory indexes?
             (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
             (elnode-http-return httpcon index))
         ;; It's a file... use 'cat' to send it to the user
         (if (file-exists-p targetfile)
             (progn
               (mailcap-parse-mimetypes)
               (let ((mimetype (or (car (rassoc
                                         (cadr (split-string targetfile "\\."))
                                         mime-types))
                                   (mm-default-file-encoding targetfile)
                                   "application/octet-stream")))
                 (elnode-http-start httpcon 200 `("Content-type" . ,mimetype))
                 (elnode-child-process httpcon "cat" targetfile)))
           ;; FIXME: This needs improving so we can handle the 404
           ;; This function should raise an exception?
           (elnode-send-404 httpcon)))))))

(defun elnode-webserver-handler-maker (&optional docroot extra-mime-types)
  "Make a webserver handler possibly with the specific 'docroot' and 'extra-mime-types'.

Returns a proc which is the handler. The handler serves files out
of the docroot and marks them with the content types that Emacs
knows about. You can add extra content types for the webserver
just by supplying an alist of mime-types and extensions for
'extra-mime-types'.

The webserver handler also creates file indexes.

The webserver uses 'elnode-test-path' to make sure that the
request does not go above the 'docroot'."
  ;;; REQUIRES LEXICAL SCOPE
  (let ((my-docroot (or docroot elnode-webserver-docroot))
        (my-mime-types (or extra-mime-types
                           elnode-webserver-extra-mimetypes)))
    ;; Return the proc
    (lambda (httpcon)
      (elnode--webserver-handler-proc httpcon my-docroot my-mime-types))))

;;;###autoload
(defun elnode-webserver (httpcon)
  "A very simple default webserver that serves documents out of 'elnode-webserver-docroot'.

This is just an example of an elnode webserver, but it may be all
that is needed most of the time.

See 'elnode-webserver-handler-maker' for more possibilities for
making webserver functions."
  (elnode--webserver-handler-proc
   httpcon
   elnode-webserver-docroot
   elnode-webserver-extra-mimetypes))


;;;###autoload
(defcustom elnode-init-port 8000
  "The port that 'elnode-init' starts the default server on."
  :group 'elnode)

(defcustom elnode-init-host "localhost"
  "The host that 'elnode-init' starts the default server listening on."
  :group 'elnode)

;;;###autoload
(defun elnode-init ()
  "Bootstraps the elnode environment when the Lisp is loaded.

It's useful to have elnode start automatically... on Lisp
load.  If the variable 'elnode-init-port' is set then this
function will launch a server on it.

The server is started with 'elnode-hostpath-default-handler' as
the handler and listening on 'elnode-init-host'"
  (if elnode-init-port
      (condition-case nil
          (elnode-start 'elnode-hostpath-default-handler elnode-init-port elnode-init-host)
        (error (message
                "elnode can't start because port %d has something attached already"
                elnode-init-port))))
  ;;(if (not elnode--defer-timer)
  ;;    (elnode--init-deferring))
  )

;; Auto start elnode
(elnode-init)

(provide 'elnode)

;; elnode.el ends here

(provide 'elnode)

;;; elnode.el ends here
