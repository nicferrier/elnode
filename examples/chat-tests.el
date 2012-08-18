;;;

(ert-deftest chat-list-since ()
  "Test that the since and add stuff works."
  (flet ((select (seq &rest indeces)
           "Select multiple elements from a list."
           (loop for i in indeces
              if (elt seq i)
              collect (elt seq i))))
    (let ((chat-list '())
          (t1 (current-time)))
      (chat-add "nic" "hello everyone!")
      (let ((cls (chat-list-since t1)))
        (should
         (equal
          (select (car cls) 1 2)
          '("nic" "hello everyone!")))))))


;;; chat-tests.el ends
