;;; upload example with elnode

;; Start a server with this handler on port 8011 and use curl:
;; 
;; curl -si -Fmyfile=@chat.css -Fthing=nic http://localhost:8011/
;;
;; chat.css is from the example directory

(require 'elnode)

(defun upload (httpcon)
  (elnode-http-start httpcon 200 '(Content-type . "text/plain"))
  (let ((data (elnode-http-param httpcon "thing"))
        (file (elnode-http-param httpcon "myfile")))
    (elnode-http-return
     httpcon
     (concat
      "thing: " data "\n"
      "myfile(filename): " (get-text-property 0 :elnode-filename file) "\n"
      "myfile: " file "\n"))))


;;; upload.el ends here
