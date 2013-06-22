;;; elnode-proxy.el -- proxying with elnode -*- lexical-binding: t -*-

;;; Commentary:

;; This is stuff to let you make proxy servers with Elnode.


;;; Code:

(require 's)
(require 'web)
(require 'elnode)

(defun elnode--web->elnode-hdr (hdr httpcon)
  "Send the HDR from the web HTTP request to Elnode's HTTPCON."
  (let ((headers
         (-filter
          (lambda (hdr-pair)
            (unless (member
                     (downcase (symbol-name (car hdr-pair)))
                     '("status-code" "status-string" "status-version"))
              (cons (symbol-name (car hdr-pair))
                    (cdr hdr-pair))))
          (kvhash->alist hdr))))
    (apply 'elnode-http-start  httpcon 200 headers)))

(defun elnode--proxy-x-forwarded-for (httpcon)
  "Return an X-Forwaded-For header."
  (let ((ipaddr (elnode-get-remote-ipaddr httpcon))
        (hdr (elnode-http-header httpcon "X-Forwarded-For")))
    (if hdr
        (concat hdr (format ", %s" ipaddr))
        ipaddr)))

;;;###autoload
(defun elnode-make-proxy (url)
  "Make a proxy handler sending requests to URL.

What is returned is an elnode handler to send requests to the
specified URL.  The URL may include `s-format' patterns for
interpolation with any of these variables:

 path - the path from the HTTP request
 params - the params from the HTTP request
 query - the params from the HTTP request as a query

For example, \"http://myserver:8000${path}${query}\" would cause
\"myserver\" on port 8000 to get the query from the user with the
specified path and query.

A client with a specified HTTP proxy sends the full request as
the path, eg:

  GET http://somehost:port/path?query HTTP/1.1

So `elnode-make-proxy' can make (something like) a full proxy
server with:

  (elnode-make-proxy \"${path}${query}\")

There may be many things that a full proxy does that this does
not do however.

Reverse proxying is a simpler and perhaps more useful."
  (lambda (httpcon)
    (let* ((method (elnode-http-method httpcon))
           (path (elnode-http-pathinfo httpcon))
           (params (web-to-query-string
                    (elnode-http-params httpcon)))
           (params-alist
            (list
             (cons "path" path)
             (cons "query" (concat "?" params))
             (cons "params" params)))
           (web-url (s-format url 'aget params-alist))
           hdr-sent)
      (process-put
       httpcon
       :elnode-child-process
       (web-http-call
        method
        (lambda (httpc hdr data)
          (unless hdr-sent
            (elnode--web->elnode-hdr hdr httpcon)
            (setq hdr-sent t))
          (if (eq data :done)
              (elnode-http-return httpcon)
              (elnode-http-send-string httpcon data)))
        :mode 'stream
        :url web-url
        :extra-headers
        `(("X-Forwarded-For"
           . ,(elnode--proxy-x-forwarded-for httpcon))))))))

(defvar elnode--proxy-server-port-history nil
  "History variable used for proxy server port reading.")

(defvar elnode--proxy-server-goto-url-history nil
  "History variable used for proxy goto urls.")

;;;###autoload
(defun elnode-make-proxy-server (port &optional url)
  "Make a proxy server on the specified PORT.

Optionally have requests go to URL.  If URL is not specified it
is \"${path}${query}\".

Interactively use C-u to specify the URL."
  (interactive
   (list
    (read-from-minibuffer
     "proxy server port:" nil nil nil
     'elnode--proxy-server-port-history)
    (if current-prefix-arg
        (read-from-minibuffer
         "proxy server goto url:" "${path}${query}" nil nil
         'elnode--proxy-server-goto-url-history
         "${path}${query}")
        "${path}${query}")))
  (let ((proxy-handler
         (elnode-make-proxy (or url "${path}${query}"))))
    (elnode-start proxy-handler :port port)))


(defun elnode-send-proxy-redirect (httpcon location)
  "Send back a proxy redirect to LOCATION.

A proxy redirect is setting \"X-Accel-Redirect\" to a location,
proxies can interpret the header with some kind of internal only
URL resolution mechanism and do dispatch to another backend
without sending the redirect back to the origin UA."
  (elnode-http-header-set
   httpcon "X-Accel-Redirect" location)
  ;; This is an nginx specific hack because it seems nginx kills the
  ;; socket once the accel header arrives
  (condition-case err
      (elnode-send-redirect httpcon location)
    (error (unless (string-match
                    "\\(SIGPIPE\\|no longer connected\\)"
                    (format "%s" (cdr err)))
             (signal (car err) (cdr err))))))

(defun elnode-send-proxy-location (httpcon location)
  "Send LOCATION with proxying techniques.

If the HTTPCON comes from a proxy (detected by checking the
\"X-Forwarded-For\") then an `elnode-send-proxy-redirect' to
location is sent.

Alternately it sets up a direct proxy call to the current server
for the location."
  (if (elnode-http-header httpcon "X-Forwarded-For")
      (elnode-send-proxy-redirect httpcon location)
      ;; Else we're not behind a proxy, send a proxy version
      (let* ((server (elnode-server-info httpcon))
             (url (format "http://%s%s" server location)))
        (funcall (elnode-make-proxy url) httpcon))))

(defun* elnode-proxy-post (httpcon path
                                   &key (mode 'batch)
                                   callback data extra-headers)
  "Make an HTTP call to localhost or the first upstream proxy."
  
  (let* ((hp-pair
          (if (elnode-http-header httpcon "X-Forwarded-For")
              (elnode-get-remote-ipaddr httpcon)
              (elnode-server-info httpcon)))
         (url (format "http://%s%s" hp-pair path)))
    (web-http-post
     (or callback
         (lambda (httpc hdr data)
           (elnode-error
            "%s post response %S %s"
            httpcon hdr data)))
     :url url :mode mode :data data
     :extra-headers extra-headers)))

(provide 'elnode-proxy)

;;; elnode-proxy.el ends here
