;; Sets the package store to a dummy location

(setq package-user-dir
      (concat
       (file-name-directory
        (or (buffer-file-name)
            load-file-name
            default-directory))
       ".elpa"))
(load-file "build.el")

;; Load all the files in the list
(flet ((map-regex (buffer regex fn)
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-min))
             (let (res)
               (save-match-data
                 (while (re-search-forward regex nil t)
                   (let ((f (match-data)))
                     (setq res
                           (append res
                                   (list
                                    (save-match-data
                                      (funcall fn f))))))))
               res)))))
  (map-regex
   (find-file-noselect
    (concat
     (file-name-directory
      (or (buffer-file-name)
          load-file-name
          default-directory)) "build-parts.txt"))
   "^\\(.*.el\\(\\.gz\\)*\\)$"
   (lambda (md)
     (let ((filename (match-string 0)))
       (when (file-exists-p filename)
         (load-file filename))))))

;; And now run the tests
(ert-run-tests-batch-and-exit)

;; End
