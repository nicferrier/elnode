;; ert tests for elnode
;; 
;; Commentary
;;
;; I am not at all sure about ert as a means of testing. Giving it a
;; go because it's a standard package for elisp testing.

;;  Copyright (C) 2010 by Nic Ferrier

(require 'ert)
(require 'elnode)

(deftest elnode--test-webserver-index-list-item ()
  (let* ((elnode-dir 
          (file-name-directory
           (buffer-file-name 
            (car
             (save-excursion 
               (find-definition-noselect 'elnode--webserver-index-list nil))))))
         (test-docroot (concat elnode-dir "test_docroot")))
    (should (equal "<a href='/example.html'>example.html</a><br/>\r\n"
                   (elnode--webserver-index-list-item
                    test-docroot
                    (concat test-docroot "example.html")
                    "/example.html"
                    '("example.html"))))))

;; End
