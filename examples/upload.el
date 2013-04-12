;;; upload example with elnode

(require 'elnode)

(defun upload (httpcon)
  (elnode-http-start httpcon 200 '(Content-type . "text/plain"))
  (let ((data (elnode-http-param httpcon "thing"))
        (file (elnode-http-param httpcon "myfile")))
    (elnode-http-return httpcon (concat data "\n" file "\n"))))

