;; Batch test Elnode

(setq package-dir
      (concat
       (file-name-directory
        (or (buffer-file-name)
            load-file-name))
       "elpa"))
(setq package-cache-dir
      (concat package-dir "/../elpacache"))

(when (file-exists-p package-dir)
  (delete-directory package-dir t))

(setq package-user-dir package-cache-dir)
(setq package-archives
      '(("local" . package-cache-dir)
        ("gnu" . "http://elpa.gnu.org/packages/")
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
