;;; elnode-proxy.el -- proxying with elnode -*- lexical-binding: t -*-

;;; Commentary:

;; This is stuff to let you make proxy servers with Elnode.


;;; Code:

(require 's)
(require 'web)
(require 'elnode)

(defun elnode/web->elnode-hdr (hdr httpcon)
  "Send the HDR from the web HTTP request to Elnode's HTTPCON."
  (apply
   'elnode-http-start
   httpcon 200
   (mapcar
    (lambda (hdr-pair)
      (cons (symbol-name (car hdr-pair))
            (cdr hdr-pair)))
    (kvhash->alist hdr))))

(defun elnode/get-remote-ipaddr (httpcon)
  "Return the remote IP address from the HTTPCON."
  (let* ((remote (plist-get
                  (process-contact httpcon t)
                  :remote))
         (ip-addr (mapcar 'identity remote)))
    (destructuring-bind (a b c d port) ip-addr
      (format "%s.%s.%s.%s:%s" a b c d port))))

(defun elnode/proxy-x-forwarded-for (httpcon)
  "Return an X-Forwaded-For header."
  (let ((ipaddr (elnode/get-remote-ipaddr httpcon))
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
            (elnode/web->elnode-hdr hdr httpcon)
            (setq hdr/sent t))
          (if (eq data :done)
              (elnode-http-return httpcon)
              (elnode-http-send-string httpcon data)))
        :mode 'stream
        :url web-url
        :extra-headers
        `(("X-Forwarded-For"
           . ,(elnode/proxy-x-forwarded-for httpcon))))))))

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

(provide 'elnode-proxy)

;;; elnode-proxy.el ends here
