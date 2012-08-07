;; Batch test Elnode

(setq package-user-dir
      (concat
       (file-name-directory
        (or (buffer-file-name)
            load-file-name
            default-directory))
       ".elpa"))
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(package-refresh-contents)
(let ((tar-package
       (concat
        (file-name-directory
         (or (buffer-file-name)
             load-file-name))
        (car (reverse command-line-args)))))
  (message "the package is: %s" tar-package)
  (package-install-file tar-package))

;; End
