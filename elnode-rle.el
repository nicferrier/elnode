;;; elnode-rle.el --- Remote Lisp Executiion with Elnode  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier
;; Keywords: lisp, hypermedia, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'elnode)
(require 'web)
(require 'loadhist)

(defun elnode-rle--handler (httpcon)
  "Remote Lisp Evaluator handler.

This can be spawned in a client to allow any lisp code to be
passed over the client-server link."
  (let ((lisp (elnode-http-param httpcon "lisp")))
    (elnode-http-start httpcon 200 '("Content-type" . "text/plain"))
    (with-stdout-to-elnode httpcon
        (eval (car (read-from-string lisp))))))

(ert-deftest elnode-rle--handler ()
  "Test the Remote Lisp Evaluator handler."
  (let
      ((lisp
        `("lisp" .
                 ,(format
                   "%S"
                   '(let ((a "hello world!"))
                     (princ a))))))
    (should
     (equal
      "c\r\nhello world!\r\n" ; is this right??
      (fakir-mock-process
          :httpcon
          ((:elnode-http-params (list lisp)))
        (elnode-rle--handler :httpcon)
        (with-current-buffer (process-buffer :httpcon)
          (buffer-substring (point-min) (point-max))))))))

(defvar elnode-rle--servers (make-hash-table :test 'equal)
  "The hash of RLE servers available.")

(defun elnode-rle--load-path-ize (lisp)
  "Wrap LISP in the current load-path."
  (concat
   ;; There is a very strange thing with sending lisp to
   ;; (read) over a piped stream... (read) can't cope with
   ;; multiple lines; so we encode newline here.
   ;;(replace-regexp-in-string
   ;; "\n"
   ;; "\\\\n"
   (format "(progn (setq load-path (quote %S)) %s)"
           (append (list default-directory) load-path)
           lisp)))

(defun elnode-rle--handler-lisp (to-require)
  "Return a file with Lisp to start Elnode with TO-REQUIRE.

Used to construct the lisp to send.  You're unlikely to need to
override this at all, the function is just here to make the
implementation easier to debug.

TO-REQUIRE is a list of things to require, currently only 1 is
allowed."
  (let ((temp-file
         (make-temp-file
          (format "elnode-rle-%s" (symbol-name to-require)))))
    (with-temp-file temp-file
      (insert
       (elnode-rle--load-path-ize
        (format "(progn
 (setq elnode-do-init nil)
 (setq elnode--do-error-logging nil)
 (require (quote %s))
 (toggle-debug-on-error)
 (load-library \"elnode-rle\")
 (let ((port (elnode-find-free-service)))
   (elnode-start 'elnode-rle--handler :port port)
   (print (format \"\\nelnode-port=%%d\\n\" port))))
 (while t (sit-for 60)))"
                to-require))))
    temp-file))

(defun elnode-rle--client-data-mapper (con hdr data stream)
  "Recevies data from the RLE server and sends it to the STREAM."
  ;; See comment in elnode-rle--call-mapper - this is gonna get MUCH
  ;; more complicated; we need to handle different types of STREAM
  ;;
  ;; STREAM may be an HTTP connection and we could support that
  ;; natively but it may also be a buffer or a marker or REAL stdout
  ;; or a non-http connection process

  ;; If we're called we must have a header so work out if we need to
  ;; send it
  ;;
  ;; What does this mean for something that's not an HTTP connection?
  (cond
    ((processp stream)
     (unless (process-get stream :header-sent)
       (elnode-http-start stream
                          (gethash 'status-code hdr))
       (process-put stream :header-sent t))
     (if (not (eq data :done))
         (elnode-http-send-string stream data)
         ;; Else we return and delete the con coz we finished
         (elnode-http-return stream)
         (delete-process con)))
    ((bufferp stream)
     (if (not (eq data :done))
         (with-current-buffer stream
           (save-excursion
             (goto-char (point-max))
             (insert data)))
         (delete-process con)))))

(defun elnode-rle--call-mapper (data stream port)
  "Make a client call to PORT mapping response to STREAM."
  (web-http-post
   (lambda (con hdr data)
     ;; FIXME - don't wanna do this - ALL this logic in that function
     ;; should be on functions on the stream
     ;;
     ;; we can attach those functions to the httpcon from something
     ;; that is aware of the context of the httpcon/stream.
     ;;
     ;; MAYBE that attachment point has to be filter.  What did we do
     ;; for the old system?
     ;;
     ;; See the comment in elnode-rle--client-data-mapper
     (elnode-rle--client-data-mapper con hdr data stream))
   "/"
   :host "localhost"
   :port port
   :data data
   :mime-type "application/x-elnode"
   :mode 'stream))

;; Setup some errors for the rle handler proc
(progn
  (put 'elnode-rle-child-port
       'error-conditions
       '(error elnode elnode-rle elnode-rle-child-port))
  (put 'elnode-rle-child-port
       'error-message
       "Elnode child process did not start a port"))

(defun elnode-rle--make-server (to-require)
  "Make an RLE server, a child Emacs running the RLE handler.

Return a proc that represents the child process.  The child
process has a property `:exec' which is a function that calls the
RLE handler in the child's Elnode server (waiting for the server
to start first and provide the relevant port) by calling
`elnode-rle-call-mapper' with the stream from the `:exec' call
and the child's remote HTTP port.

The `:exec' proc will signal `elnode-rle-child-port' if the child
server does not start properly."  ; yes. I know it's bloody complicated.
  (let* ((proc-buffer
          (get-buffer-create
           (format "* %s *" "thingy")))
         (emacsrun
          (format
           "emacs -Q  -batch -l %s"
           (elnode-rle--handler-lisp
            to-require)))
         (proc
          (start-process-shell-command
           "elnode-rle-server"
           proc-buffer
           emacsrun)))
    ;; Collect the port from the remote Emacs
    ;; - FIXME this should also collect the secure token
    (set-process-filter
     proc
     (lambda (proc data)
       ;; Optional delay for test reasons
       (with-current-buffer (process-buffer proc)
         (mapc
          (lambda (line)
            (message "> %s" line)) (split-string data "\n"))
         (save-excursion
           (goto-char (point-max))
           (insert data)
           (unless (process-get proc :port)
             (when (re-search-backward "^elnode-port=\\([0-9]+\\)$" nil t)
               (process-put proc :port (match-string 1))))))))
    ;; Make a handler to call the server
    (process-put
     proc :exec
     (lambda (data stream)
       ;; FIXME - needs to check for the secure key
       ;;
       ;; check for the port from the child... if it doesn't arrive
       ;; signal failure
       (unless (catch 'break
                 (dotimes (i 3)
                   (let ((collected-port (process-get proc :port)))
                     (when collected-port
                       (throw 'break collected-port)))
                   (sit-for 1)))
         ;; It failed to collect the port after 10 seconds
         (signal elnode-rle-child-port process))
       (let ((ephemeral-port (process-get proc :port)))
         (elnode-rle--call-mapper data stream ephemeral-port))))
    proc))

(ert-deftest elnode-rle--make-server ()
  "Test making the server."
  (should
   (with-temp-buffer
     (let* ((elnode-rle--make-server-read-port-delay 20)
            (child-proc (elnode-rle--make-server 'elnode)))
       (unwind-protect
            (condition-case var
                (progn
                  (funcall
                   (process-get child-proc :exec)
                   (let ((h (make-hash-table :test 'equal)))
                     (puthash "bindings" "((a \"hello\"))" h)
                     (puthash "lisp" "(message \"hello\")" h)
                     h)
                   (current-buffer))
                  (sit-for 4))
              (elnode-rle-child-port
               (message "server did not start with a port")))
         ;; Kill the process - we don't need it
         (delete-process child-proc))))))

(defun elnode-rle--sender (stream to-require bindings body)
  "Make a call using a client to the RLE server elsewhere.

The RLE server is reused over TO-REQUIRE, if it's not already
existing, it is created."
  (let ((server (gethash to-require elnode-rle--servers)))
    ;; Make it if it's not there
    (unless server
      (setq server
            (puthash to-require
                     (elnode-rle--make-server to-require)
                     elnode-rle--servers)))
    ;; Now make the call to the server
    (let ((data (make-hash-table :test 'equal)))
      (puthash "bindings" bindings data)
      (puthash "lisp" body data)
      (funcall
       (process-get :exec server)
       data
       stream))))

(defmacro elnode-async-do (remote
                           stream
                           with-environment bindings
                           do &rest body)
  "Execute the BODY in the REMOTE Emacs.

The STREAM is used to handle any output from the REMOTE.

The BINDINGS are also sent to the remote.

TODO

security for the remote using the stored key."
  (let ((bodyv (make-symbol "body"))
        (bindsv (make-symbol "binds"))
        (streamv (make-symbol "streamv"))
        (providev (make-symbol "provide")))
    `(let* ((,streamv ,stream)
            (,bodyv (list ,body))
            (,bindsv (list
                      ,@(loop for p in bindings
                           collect
                             (if (and p (listp p))
                                 (list 'list `(quote ,(car p)) (cadr p))
                                 (list 'cons `,p nil)))))
            (,providev (file-provides (or (buffer-file-name)
                                          load-file-name))))
       (elnode-rle--sender ,streamv ,providev ,bindsv ,bodyv))))

(provide 'elnode-rle)

;; elnode-rle ends here
