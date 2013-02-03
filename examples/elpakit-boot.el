;;; Booting elnode in a daemon

;; A demonstration of using elpakit to start elnode in a separate
;; daemon server.

;; Elpakit builds packages and does things with them, puts them into
;; local archives, evals them, runs tests on them.

(elpakit-start-server
 (list ;; A single element list with the repo in it
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (buffer-file-name)))))
 'elnode)

;;; elpakit-boot.el ends here.
