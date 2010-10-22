;;; A simple emacs async http server
;;; this is like node.js, but for elisp (obviously)
;;; (C) Nic Ferrier, 2010


;; Style note
;; This codes uses the emacs style of elnode--... for private functions.

(defvar elnode-server-socket nil
  "Where we store the server sockets.

This is an alist of proc->server-process.")


;; Main control functions

(defun elnode--sentinel (process status)
  "Sentinel function for the main server and for the client sockets"
  (cond
   ;; Server status
   ((and 
     ;;(equal process elnode-server-socket)
     (assoc (process-contact process :service) elnode-server-socket)
     (equal status "deleted\n"))
    (kill-buffer (process-buffer process))
    (message "elnode server stopped"))

   ;; Client socket status
   ((equal status "connection broken by remote peer\n")
    (if (process-buffer process)
        (kill-buffer (process-buffer process)))
    ;;(message "elnode connection dropped")
    )

   ((equal status "open\n") ;; this says "open from ..."
    (message "elnode opened new connection"))

   ;; Default
   (t
    (message "elnode status: %s %s" process status))
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
              (let* ((proc-name (process-name process))
                     (port-match 
                      (string-match "\\*elnode-webserver-proc\\* <127.0.0.1:\\([0-9]+\\)>" proc-name)))
                (set-process-buffer 
                 process 
                 (get-buffer-create (concat " *elnode-request-" (match-string 1 proc-name) "*")))
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
              (condition-case nil
                  (funcall (process-get server :elnode-http-handler) process)
                ('t 
                 ;; Try and send a 500 error response
                 (process-send-string process "HTTP/1.1 500 Server-Error\r\n<h1>Server Error</h1>\r\n"))
              )
          )
        )
      ))))

(defun elnode--log-fn (server con msg)
  "Log function for elnode.

Serves only to connect the server process to the client processes"
  (process-put con :server server)
  )

(defun elnode-start (request-handler port)
  "Start the elnode server.

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
 (elnode-start 'nic-server)
 ;; End
"
  (interactive "aHandler function: \nnPort: ")
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
                      :host 'local
                      :service port
                      :family 'ipv4
                      :filter 'elnode--filter
                      :sentinel 'elnode--sentinel
                      :log 'elnode--log-fn
                      :plist `(:elnode-http-handler ,request-handler)
                      )
                     ))
             elnode-server-socket))
    )
  )

;; TODO: make this take an argument for the 
(defun elnode-stop (port)
  "Stop the elnode server"
  (interactive "nPort: ")
  (let ((server (assoc port elnode-server-socket)))
    (if server
        (progn
          (delete-process (cdr server))
          (setq elnode-server-socket 
                (remove-if 
                 (lambda (elem) (equal (car elem) port))
                 elnode-server-socket))))))


(defun elnode-list-buffers ()
  "List the current buffers being managed by elnode"
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

 (http-status . http-header-alist)
"
  (with-current-buffer (process-buffer httpcon)
    ;; TODO improve this parsing - we need to record the end of
    ;; header so we can use that instead of point-max (when we're
    ;; parsing POST bodys the point-max method will be no good)
    (let* ((lines (split-string (buffer-substring (point-min) (point-max)) "\r\n" 't))
           (status (car lines))
           (header (cdr lines)))
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
   (process-get httpcon :elnode-http-header)
   )
  )

(defun elnode-http-header (httpcon name)
  "Get the header specified by name from the header"
  (let ((hdr (or 
              (process-get httpcon :elnode-http-header)
              (cdr (elnode--http-parse httpcon)))))
    (cdr (assoc name hdr))
    ))



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
        (process-get httpcon property))
    )
  ) 

(defun elnode--http-parse-resource (httpcon &optional property)
  "Convert the specified resource to a path and a query"
  (let ((resource 
         (or
          (process-get httpcon :elnode-http-resource)
          (elnode--http-parse-status httpcon :elnode-http-resource))))
    (or (string-match "^\\(/\\)\\(\\?.*\\)*$" resource)
        (string-match "^\\(/[A-Za-z0-9_/.-]+\\)\\(\\?.*\\)*$" resource))
    (process-put httpcon :elnode-http-pathinfo (match-string 1 resource))
    (if (match-string 2 resource)
        (let ((query (match-string 2 resource)))
          (string-match "\\?\\(.+\\)" query)
          (if (match-string 1 query)
              (process-put httpcon :elnode-http-query (match-string 1 query))))))
  (if property
      (elnode--http-parse-status httpcon property))
    )

(defun elnode-http-pathinfo (httpcon)
  "Get the PATHINFO of the request"
  (or
   (process-get httpcon :elnode-http-pathinfo)
   (elnode--http-parse-resource httpcon :elnode-http-pathinfo)))

(defun elnode-http-query (httpcon)
  "Get the QUERY of the request"
  (or
   (process-get httpcon :elnode-http-query)
   (elnode--http-parse-resource httpcon :elnode-http-query)))

(defun elnode-http-params (httpcon)
  "Get an alist of the parameters in the request"
  (or 
   (process-get httpcon :elnode-http-params)
   (let ((query (elnode-http-query httpcon)))
     (if query
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
           (process-put httpcon :elnode-http-params alist)
           alist)
       ;; Else just return nil
       '()
       ))))

(defun elnode-http-method (httpcon)
  "Get the PATHINFO of the request"
  (or
   (process-get httpcon :elnode-http-method)
   (elnode--http-parse-status httpcon :elnode-http-method)))

(defun elnode-http-version (httpcon)
  "Get the PATHINFO of the request"
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
  (let ((http-codes-strings '(("200" . "Ok")
                              (200 . "Ok")
                              ("400" . "Bad Request")
                              (400 . "Bad Request")
                              ("401" . "Authenticate")
                              (401 . "Authenticate")
                              ("404" . "Not Found")
                              (404 . "Not Found")
                              ("500" . "Server Error")
                              (500 . "Server Error")
                              )))
    ;; Send the header
    (let ((header-alist (cons 
                         '("Transfer-encoding" . "chunked")
                         header)))
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
         "\r\n")
        ))))
  )

(defun elnode-http-return (httpcon data)
  "End the http response on the specified http connection

httpcon is the http connection.
data must be a string right now."
  (elnode-http-send-string httpcon data)
  ;; Need to close the chunked encoding here
  (elnode-http-send-string httpcon "")
  (process-send-string httpcon "\r\n")
  (delete-process httpcon)
  )


(defun elnode--mapper-find (path url-mapping-table)
  "Try and find the path inside the url-mapping-table.

Implementation notes: This is basically the standard emacs filter
with a fixed url-match filter function."
  (message "elnode--mapper-find path: %s" path)
  (car (delq nil
             (mapcar (lambda (mapping)
                       (and (string-match 
                             (format "^/%s" (car mapping)) 
                             path)
                            mapping)) url-mapping-table))))

(defun elnode-test-mapper-find ()
  "Just a test function for the mapper"
  (let ((mt '(("$" . (lambda (h) "root matched!"))
              ("me/$" . (lambda (h) "me matched!"))
              ("nicferrier/$" . (lambda (h) "hi nic")))))
    ;; Test the paths in the list
    (mapconcat
     (lambda (namepath)
       (let ((path (car namepath))
             (name (cdr namepath)))
         (let ((m (elnode--mapper-find path mt)))
           (if (and m (functionp (cdr m)))
               (funcall (cdr m) 't)
             (format "%s failed to match" name)))))
     '(("/" . "root") ("/me/" . "me") ("/nicferrier/" . "hi nic"))
     "\n"
     )))

(defun elnode-handler-404 (httpcon)
  "A generic 404 handler"
  (elnode-http-start httpcon 404 '("Content-type" . "text/html"))
  (elnode-http-return httpcon "<h1>Not Found</h1>\r\n"))


(defun elnode-dispatcher (httpcon url-mapping-table &optional function-404)
  "Dispatch the request to the correct function based on the mapping table.

url-mapping-table is an alist of url-regex . function-to-dispatch.
"
  (let ((m (elnode--mapper-find (elnode-http-pathinfo httpcon) url-mapping-table)))
    (if (and m (functionp (caddr m)))
        (funcall (caddr m) httpcon)
      ;; We didn't match so fire a 404... possibly a custom 404
      (if (functionp function-404)
          (funcall function-404 httpcon)
        ;; We don't have a custom 404 so send our own
        (elnode-handler-404 httpcon)
        ))
    ))


;; elnode child process functions

;; TODO: handle errors better than messaging
(defun elnode-child-process-sentinel (process status)
  "A generic sentinel for elnode child processes.

elnode child processes are just emacs asynchronous processes that
send their output to an elnode http connection.

The main job of this sentinel is to send the end of the http
stream when the child process finishes."
  (cond
   ((equal status "finished\n")
    (let ((httpcon (process-get process :elnode-httpcon)))
      (elnode-http-send-string httpcon  "")
      (process-send-string httpcon "\r\n")
      (delete-process httpcon)))
   (t 
    (message "elnode-chlild-process-sentinel: %s" status)
    )
   )
  )

(defun elnode-child-process-filter (process data)
  "A generic filter function for elnode child processes.

elnode child processes are just emacs asynchronous processes that
send their output to an elnode http connection.

This filter function does the job of taking the output from the
async process and finding the associated elnode http connection
and sending the data there."
  (let ((httpcon (process-get process :elnode-httpcon)))
    (elnode-http-send-string httpcon data)
    )
  )

(defun elnode-child-process (httpcon program &rest args)
  "Run the specified process asynchronously and send it's output to the http connection.

program is the program to run.
args is a list of arguments to pass to the program."
  (let* ((args `(,(format "%s-%s" (process-name httpcon) program)
                 ,(format " %s-%s" (process-name httpcon) program)
                 ,program
                 ,@args
                ))
         (p (apply 'start-process args)))
    ;; Bind the http connection to the process
    (process-put p :elnode-httpcon httpcon)
    (set-process-filter p 'elnode-child-process-filter)
    (set-process-sentinel p 'elnode-child-process-sentinel)
    ))

;; Webserver stuff

(defcustom elnode-webserver-docroot "/home/nferrier/elnode"
  "the document root of the webserver.")

(defcustom elnode-webserver-extra-mimetypes '(("text/plain" . "creole")
                                               ("text/plain" . "el"))
  "this is just a way of hacking the mime type discovery so we
  can add more file mappings more easily than editing
  /etc/mime.types")

(defun elnode-webserver-handler-maker (&optional docroot extra-mime-types)
  "Make a webserver handler possibly with the specific docroot and extra-mime-types

Returns a proc which is the handler."
  (lexical-let ((my-docroot (or 
                             docroot
                             elnode-webserver-docroot))
                (my-mime-types (or extra-mime-types
                                   elnode-webserver-extra-mimetypes)))
    ;; Return the proc
    (lambda (httpcon)
      (let* ((pathinfo (elnode-http-pathinfo httpcon))
             (targetfile (format "%s%s" 
                                 my-docroot 
                                 (if (equal pathinfo "/")  "" pathinfo))))
        (if (not (or 
                  (file-exists-p targetfile)
                  ;; Test the targetfile is under the docroot
                  (compare-strings           
                   my-docroot 0 (length my-docroot)
                   (file-truename targetfile) 0 (length docroot)
                   )
                  ))
            (elnode-handler-404 httpcon)
          ;; The file exists and is legal
          (if (file-directory-p targetfile)
              ;; What's the best way to do simple directory indexes?
              (let* ((dirlist (directory-files-and-attributes targetfile))
                     (html-dir (format "<html><head><title>%s</title></head><body><h1>%s</h1><div>%s</div></body></html>"
                                       pathinfo
                                       pathinfo
                                       (mapconcat 
                                        (lambda (dir-entry)
                                          (format "<a href='%s'>%s</a><br/>\r\n" (car dir-entry) (car dir-entry))
                                          )
                                        dirlist 
                                        "\n"))))
                (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
                (elnode-http-return httpcon html-dir))
            (progn
              (require 'mailcap)
              (mailcap-parse-mimetypes)
              (let ((mimetype (or (car (rassoc 
                                        (cadr (split-string targetfile "\\."))
                                        my-mime-types))
                                  (mm-default-file-encoding targetfile)
                                  "application/octet-stream")))
                (elnode-http-start httpcon 200 `("Content-type" . ,mimetype))
                (elnode-child-process httpcon "cat" targetfile)
                )
              )))))))



;; Demo handlers

(defun nicferrier-handler (httpcon)
  "Demonstration function.

This is a simple handler that just sends some HTML in response to
any request."
  (let* ((host (elnode-http-header httpcon "Host"))
         (pathinfo (elnode-http-pathinfo httpcon))
         )
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (elnode-http-return 
     httpcon 
     (format 
      "<html>
<body>
<h1>%s</h1>
<b>HELLO @ %s %s %s</b>
</body>
</html>
" 
      (or (cdr (assoc "name" (elnode-http-params httpcon))) "no name")
      host 
      pathinfo 
      (elnode-http-version httpcon)
      ))
    )
  )

(defun nicferrier-process-handler (httpcon)
  "Demonstration function

This is a handler based on an asynchronous process."
  (let* ((host (elnode-http-header httpcon "Host"))
         (pathinfo (elnode-http-pathinfo httpcon))
         )
    (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
    (elnode-child-process httpcon "cat" "/home/nferrier/elnode/example.html")
    )
  )

(defun nicferrier-process-webserver (httpcon)
  "Demonstration webserver.

Shows how to use elnode's built in webserver toolkit to make
something that will serve a docroot."
  (let ((webserver (elnode-webserver-handler-maker)))
    (funcall webserver httpcon))
  )

(defun nicferrier-mapper-handler (httpcon)
  "Demonstration function

Shows how a handler can contain a dispatcher to make it simple to
handle more complex requests."
  (elnode-dispatcher httpcon
                     '(("/$" . 'nicferrier-handler)
                       ("nicferrier/$" . 'nicferrier-handler))))

;; End
