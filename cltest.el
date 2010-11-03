;; These are simple tests for elnode based on the cl library
;; This all very much needs a framework... but these are at least tests.

(require 'elnode)
(require 'cl)

;; Test elnode-http-post-to-alist
(let ((test-name "elnodetest-elnode--http-post-to-alist"))
  (assert 
   (equal '(("a" . "1")("b" . "2")("c" . "3"))
          (progn
            (kill-buffer (get-buffer-create (format "*%s*" test-name)))
            (let ((buf (get-buffer-create (format "*%s*" test-name))))
              (with-current-buffer buf
                (insert "GET / HTTP/1.1\r\nHost: www\r\n\r\na=1&b=2&c=3"))
              (flet ((process-buffer (process) buf)
                     (process-get (process key)
                                  (cond
                                   ((eq key :elnode-header-end) 30)
                                   ('t (error "unknown key")))))
                (elnode--http-post-to-alist "")))))
          't))

;; Test elnode-http-params somewhat
(let ((test-name "elnodetest-elnode-http-params"))
  (assert 
   (equal '(("a" "10" "1")("b" "40" "2")("c" . "3"))
          (sort 
           (progn
             (kill-buffer (get-buffer-create (format "*%s*" test-name)))
             ;; Define a buffer and some contents for emulating POST data
             (let ((buf (get-buffer-create (format "*%s*" test-name))))
               (with-current-buffer buf
                 ;; We define the POST data here
                 (insert "GET / HTTP/1.1\r\nHost: www\r\n\r\na=1&b=2&c=3"))
               (flet ((process-buffer (process) buf)
                      (process-get (process key)
                                   (cond
                                    ;; We define the query here
                                    ((eq key :elnode-http-query) "a=10&b=40")
                                    ((eq key :elnode-http-params) nil)
                                    ((eq key :elnode-header-end) 30)
                                    ((eq key :elnode-http-method) "POST")
                                    ('t (error "unknown key"))))
                      (process-put (process key value) value))
                 (elnode-http-params "")))) 
           (lambda (x y) (string-lessp (car x)(car y)))))
          't))
                       
;; End
