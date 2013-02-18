;;; chat example - very simple webchat -*- lexical-binding: t -*-

(elnode-app chat-dir esxml)

(defvar chat-list '())

(defun chat-add (user text)
  (add-to-list
   'chat-list
    (list (current-time) user text)))

(defun chat-list-since (since)
  (loop for rec in chat-list
       if (time-less-p since (car rec))
       collect rec))

;; And now the elnode

(defun chat-comet-handler (httpcon)
  "Defer until there is new chat."
  ;; FIXME - when this breaks it seems to be down to lexical-binding
  ;; not being true
  (let ((entered (current-time)))
    (elnode-defer-until (chat-list-since entered)
        (elnode-send-json
         httpcon
         elnode-defer-guard-it :jsonp t))))

(defun chat-send-handler (httpcon)
  (let* ((username (elnode-http-cookie httpcon "chatusername" t))
         (msg (elnode-http-param httpcon "msg")))
    (chat-add username msg)
    (elnode-send-json httpcon (json-encode '("thanks")))))

;; Main page setup stuff

(defun chat-list-to-html ()
  "Return the `chat-list' as rows for initial chat display."
  (loop for entry in chat-list
     if (equal 3 (length entry))
     concat
       (esxml-to-xml
        `(tr
          ()
          (td
           ((class ,(concat "username " (elt entry 1)))) ,(elt entry 1))
          (td ((class "message")) ,(elt entry 2))))))

(defun chat-main-templater ()
  "Return the `chat-list' as rows for initial chat display."
  (list
   (cons
    "messages"
       (let ((chat-list
           (subseq chat-list
                   0 (if (> (length chat-list) 10)
                         12
                         (length chat-list)))))
      (chat-list-to-html)))))

(defun chat-main-handler (httpcon)
  "The main handler."
  (let ((chat-js (concat chat-dir "chat.js"))
        (chat-html (concat chat-dir "chat.html"))
         (chat-css (concat chat-dir "styles.css")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^.*//chat/poll/" . chat-comet-handler)
       ("^.*//chat/send/" . chat-send-handler)
       ("^.*//chat.js" . ,(elnode-make-send-file chat-js))
       ("^.*//styles.css" . ,(elnode-make-send-file chat-css))
       ("^.*//" . ,(elnode-make-send-file
                    chat-html
                    :replacements 'chat-main-templater))))))

;;; chat.el ends here
