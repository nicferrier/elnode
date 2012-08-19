;;; chat example - very simple webchat -*- lexical-binding: t -*-

(require 'cl)

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

(require 'elnode)

(defun chat-comet-handler (httpcon)
  "Defer until there is new chat."
  (let ((callback (elnode-http-param httpcon "callback"))
        (entered (current-time)))
    (elnode-defer-until (chat-list-since entered)
        (elnode-send-json
         httpcon elnode-defer-guard-it :jsonp t))))

(defun chat-send-handler (httpcon)
  (let ((username (elnode-http-param httpcon "username"))
        (msg (elnode-http-param httpcon "msg")))
    (chat-add username msg)
    (elnode-send-json httpcon (json-encode '("thanks")))))


;; Main page setup stuff

(require 'creole)

(defconst chat-dir (file-name-directory
                    (or (buffer-file-name)
                        load-file-name
                        default-directory)))

(defun chat-page-handler (httpcon)
  "Send the chat page."
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (with-stdout-to-elnode httpcon
      (creole-wiki
       (expand-file-name
        (concat chat-dir "chat.creole"))
       :destination t)))

(defun chat-main-templater ()
  "Return the `chat-list' as rows for initial chat display."
  (list
   (cons
    "messages"
    (loop for entry in chat-list
       if (equal 3 (length entry))
       concat
         (concat "<tr><td class=\"username\">"
                 (elt entry 1)
                 "</td><td class=\"message\">"
                 (elt entry 2)
                 "</td></tr>")))))

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
