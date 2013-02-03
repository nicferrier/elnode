;;; interactive javascript shell with elnode -*- lexical-binding: t -*-

(elnode-app elnode-ijs-dir comint uuid)

(defvar elnode-ijs/input nil
  "Input for the js shell.

Each element is a cons of a uuid and the javascript to send.")

(defvar elnode-ijs/input-hash (make-hash-table :test 'equal)
  "Record of things sent to the js engine.

This stores the UUID and the js comint process for all input.
The js engine uses the UUID to communicate results back to us,
when it does the relevant element is deleted.")

(defun elnode-ijs-handler (httpcon)
  (elnode-defer-until (car-safe elnode-ijs/input)
      (unwind-protect
           (elnode-send-json httpcon elnode-ijs/input)
        (setq elnode-ijs/input nil))))

(defun elnode-ijs-result-handler (httpcon)
  (let* ((id (elnode-http-param httpcon "uuid"))
         (result (elnode-http-param httpcon "result"))
         (buffer (gethash id elnode-ijs/input-hash)))
    (remhash id elnode-ijs/input-hash)
    (if buffer
        (with-current-buffer buffer
          ;; Delete the hash
          (let ((buffer-read-only nil))
            (save-excursion
               (re-search-backward
                (concat "#" id)
                nil t)
               (delete-region (point) (line-end-position))
               (insert (format "%S" result)))))
        ;; Else
        (message "%s is not in the hash" id))))

(defun elnode-ijs/bootstrap (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return
   httpcon
   "<html>
<head>
<script src=\"//ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js\">
</script>
<script src=\"/ijs.js\" language=\"Javascript\">
</script>
</head>
<body>
</body>
</html>"))

(defun elnode-ijs/test (httpcon)
  "A test thing."
  (elnode-send-json httpcon '(just a test)))

(defun elnode-ijs-router (httpcon)
  "Main router for ijs"
  (elnode-dispatcher
   httpcon
   `(("^/boot/" . elnode-ijs/bootstrap)
     ("^/test/" . elnode-ijs/test)
     ("^/ijs\\.js" . ,(elnode-make-send-file
                       (concat elnode-ijs-dir "elnode-ijs.js")))
     ("^/ijsresult/" . elnode-ijs-result-handler)
     ("^/ijs/" . elnode-ijs-handler))))

(defconst elnode-ijs-prompt "IJS> ")

(defun elnode-ijs/input-sender (proc input)
  (let ((id (uuid-string))
        (buffer-read-only nil)
        (lb (- (line-beginning-position) 5)))
    (comint-output-filter proc (format "#%s\n" id))
    (push (cons id input) elnode-ijs/input)
    (puthash id (process-buffer proc) elnode-ijs/input-hash))
  (comint-output-filter proc elnode-ijs-prompt))

(define-derived-mode
    elnode-ijs/mode comint-mode "IJSM"
    "Run a Javascript shell."
    :syntax-table js2-mode-syntax-table
    (setq comint-prompt-regexp (concat "^" (regexp-quote elnode-ijs-prompt)))
    (setq comint-input-sender 'elnode-ijs/input-sender)

    (unless (comint-check-proc (current-buffer))
      ;; Was cat, but on non-Unix platforms that might not exist, so
      ;; use hexl instead, which is part of the Emacs distribution.
      (let ((fake-proc
             (condition-case nil
                 (start-process "ijsm" (current-buffer) "hexl")
               (file-error (start-process "ijsm" (current-buffer) "cat")))))
        (set-process-query-on-exit-flag fake-proc nil)
        ;; Add a silly header
        (insert "Interactive Javascript Mode\n")
        (set-marker
         (process-mark fake-proc) (point))
        (comint-output-filter fake-proc elnode-ijs-prompt))))

;; This needs to take a url
(defun elnode-ijs (port)
  (interactive "nEnter a port: ")
  (elnode-start 'elnode-ijs-router :port port)
  (let ((url (format "http://localhost:%d" port)))
    (browse-url (format "%s/boot/" url))
    (let ((buf
           ;; This needs to be tied to the url
           (get-buffer-create (format "*ijs-%s*" url))))
      (with-current-buffer buf
        (elnode-ijs/mode)
        (switch-to-buffer buf)))))

;;; elnode-ijs.el ends here
