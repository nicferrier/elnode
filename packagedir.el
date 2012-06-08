;; Sets the package store to a dummy location

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
(load-file "build.el")
(require 'elnode-wiki)
(require 'elnode-tests)
(require 'elnode-client)
(ert-run-tests-batch-and-exit)

;; End
