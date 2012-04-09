;; Batch test Elnode

(setq package-dir
      (concat
       (file-name-directory
        (or (buffer-file-name)
            load-file-name))
       ".elpa"))

(when (file-exists-p package-dir)
  (delete-directory package-dir t))

(setq package-user-dir package-dir)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-list-packages)

(setq elnode-init-port nil)
(let ((to-test
       (concat
        (file-name-directory
         (or (buffer-file-name)
             load-file-name))
        "elnode.el")))
  (package-install-file to-test))

(ert-run-tests-batch-and-exit)

;; End
