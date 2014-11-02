;;; elnode.el --- a simple emacs async HTTP server -*- lexical-binding: t -*-

;; Copyright (C) 2010, 2011, 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 5th October 2010
;; Keywords: lisp, http, hypermedia

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; elnode's code can be found here:
;;   http://github.com/nicferrier/elnode

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elnode--private-function
;;
;; for private functions.


;;; Commentary:
;;
;; This is an elisp version of the popular node.js asynchronous
;; webserver toolkit.
;;
;; You can define HTTP request handlers and start an HTTP server
;; attached to the handler.  Many HTTP servers can be started, each
;; must have its own TCP port.  Handlers can defer processing with a
;; signal (which allows comet style resource management)
;;
;; See elnode-start for how to start an HTTP server.

;;; Code:

(require 'fakir)
(require 'mm-encode)
(require 'mailcap)
(require 'mail-parse) ; for mail-header-parse-content-type
(require 'url-util)
(require 'kv)
(require 's)
(require 'dash)
(require 'rx)
(require 'web)
(require 'json)
(require 'db)
(require 'dired) ; needed for the setup
(require 'tabulated-list)
(require 'noflet)

(eval-when-compile (require 'cl))

(defconst ELNODE-FORM-DATA-TYPE "application/x-www-form-urlencoded"
  "The type of HTTP Form POSTs.")

(defconst http-referrer 'referer
  "Helper to bypass idiot spelling of the word `referrer'.")


;; Customization stuff

(defgroup elnode nil
  "An extensible asynchronous web server for Emacs."
  :group 'applications)

(defvar elnode-server-socket nil
  "Where we store the server sockets.

This is an alist of proc->server-process:

  (port . process)")


(defcustom elnode-init-port 8000
  "The port that `elnode-init' starts the default server on."
  :group 'elnode)

(defcustom elnode-init-host "localhost"
  "The default host for the default webserver.

Also used as the default host for `elnode-make-webserver'.

See `elnode-init' for more details."
  :group 'elnode)


;;;###autoload
(defconst elnode-config-directory
  (file-name-as-directory (expand-file-name "elnode" user-emacs-directory))
  "The config directory for elnode to store peripheral files.

This is used as a base for other constant directory or file
names (the elnode auth database is a file in this directory, the
elnode webserver has a docroot directory in this directory).

It is based on the `user-emacs-directory' which always seems to
be set, even when emacs is started with -Q.")

(defun elnode/con-lookup (con attr)
  "Dynamic lookup."
  (gethash attr (car (process-plist con))))

(defmacro elnode/con-put (con attr value &rest other)
  "Put ATTR with VALUE into an array on CON's plist.

If OTHER is specified it is other pairs of attribute and value."
  (declare (indent 1)
           (debug (sexp sexp form &rest sexp)))
  (let ((valv (make-symbol "val"))
        (conv (make-symbol "con")))
    `(let* ((,valv ,value)
            (,conv ,con)
            (convec
             (or (car (process-plist ,conv))
                 (car (set-process-plist
                       ,conv (list (make-hash-table :test 'eq)))))))
       (puthash ,attr ,valv convec)
       ,@(when other
               (loop for (name val) on other by 'cddr
                  collect `(puthash ,name ,val convec))))))

(defmacro elnode/con-get (con attr)
  "Alternative implementation of `process-get'."
  (let ((conv (make-symbol "con")))
    `(let* ((,conv ,con)
            (convec
             (or (car (process-plist ,conv))
                 (car (set-process-plist
                       ,conv (list (make-hash-table :test 'eq)))))))
       (gethash ,attr convec))))

(defun elnode/get-server-prop (process key)
  "Get the value of the KEY from the server attached to PROCESS.

Server properties are bound with `elnode-start' which sets up
`elnode--log-fn' to ensure that all sockets created have a link
back to the server."
  (let* ((server (elnode/con-get process :server)))
    (elnode/con-lookup server key)))


;; Error log handling

(defun elnode-join (&rest parts)
  "Path join the parts together.

EmacsLisp should really provide this by default."
  (let* (savedpart
         (path
          (loop for p in parts
             concat
               (when (> (length p) 0)
                 (setq savedpart p)
                 (file-name-as-directory p)))))
    (if (equal (elt savedpart (- (length savedpart) 1)) ?\/)
        path
        (substring path 0 (- (length path) 1)))))

(defun elnode--dir-setup (dir default default-file-name
                          &optional target-file-name
                          &rest other-files)
  "Install a DIR and DEFAULT-FILE-NAME if it's not setup already.

This is a packaging helper.  It helps an ELPA package install
files from it's package base into the user's Emacs.  If the DIR
is specified under `user-emacs-directory'.

DIR is the directory to install, DEFAULT is the default for that
directory, unless DIR equals DEFAULT nothing is done.

DEFAULT-FILE-NAME is the name of the file that will be installed
in DIR.  It is the expected name of the source file inside the
package.  Unless TARGET-FILE-NAME is specified it is also the
name the installed file will be given.  If the TARGET-FILE-NAME
is specified then that is the the name the file is installed as.

If OTHER-FILES is present it is treated as a list of other
filenames to copy to the DIR."
  (when  (and
          (equal
           dir
           default)
          (not (file-exists-p dir)))
    ;; Do install
    (let ((source-default-file
           (concat
            (file-name-directory
             (or (buffer-file-name)
                 (symbol-file 'elnode--dir-setup))) ; this not very portable
            ;; This should probably tie in with the makefile somehow
            default-file-name)))
      (when (and source-default-file
                 (file-exists-p source-default-file))
        (let ((to (concat
                   dir
                   (or target-file-name default-file-name))))
          (make-directory dir t)
          (message "copying %s elnode wiki default page to %s" dir to)
          (dired-copy-file source-default-file to nil)
          (when other-files
            (noflet ((resolve-filename (file)
                     (if (file-name-absolute-p file)
                         file
                         (concat
                          (file-name-directory
                           source-default-file)
                          file))))
              (loop for file in other-files
                 ;; does the file exist?
                 if (and file (file-exists-p (resolve-filename file)))
                 do
                   (dired-copy-file
                    ;; from...
                    (resolve-filename file)
                    ;; to...nd
                    (concat dir (file-name-nondirectory file))
                    nil)))))))))

(defun elnode--protected-load (feature dir)
  "Try and require FEATURE, if it fails try and load."
  (condition-case err
      (require feature)
    (file-error (progn
                  (load
                   (concat dir (symbol-name feature) ".el"))
                  (require feature)))))

;;;###autoload
(defmacro elnode-app (dir-var &rest features)
  "A macro that sets up the boring boilerplate for Elnode apps.

This sets up lexical binding, captures the module's parent
directory in DIR-VAR, requires `cl' and any other features you
list.  Use it like this:

 (elnode-app my-app-dir esxml mongo-elnode)

Once used you can access the variable `my-app-dir' as the dirname
of your module (which is useful for serving files and such)."
  (declare (indent 1))
  (let ((dir-var-v (make-symbol "dv")))
    `(let ((,dir-var-v (file-name-directory
                        (or (buffer-file-name)
                            load-file-name
                            default-directory))))
       (setq lexical-binding t)
       (defconst ,dir-var ,dir-var-v)
       (require 'cl)
       (require 'elnode)
       ,@(loop for f in features
            collect
              `(elnode--protected-load
                (quote ,f) ,dir-var-v)))))

(defcustom elnode-log-files-directory nil
  "The directory to store any Elnode log files.

If this is not-nil (in which case logs are not saved at all) it
must be the name of a directory Elnode can use for storing logs.
If a directory is specified but it does not exist it is created."
  :group 'elnode
  :type '(choice (const :tag "Off" nil)
          directory))

(defvar elnode-log-buffer-position-written 0
  "The position in the log buffer written.

This is used by `elnode-log-buffer-log' to track what has been written
so far.")

(defvar elnode-log-buffer-max-size 1000
  "Maximum number of lines of log.")

(defvar elnode-log-buffer-datetime-format "%Y-%m-%dT%H:%M:%S"
  "The date time format used by `elnode-log-buffer-log'.")

(defun elnode-log-buffer-log (text buffer-or-name &optional filename)
  "Log TEXT to the BUFFER-OR-NAME saving the buffer in FILENAME.

BUFFER-OR-NAME is either a buffer or a string naming a buffer.
FILENAME is a filename to save the buffer into.  If the FILENAME
is not specified then we try to use the filename of the
BUFFER-OR-NAME.

If neither a buffer filename nor FILENAME is specified then an
error is generated.

The TEXT is logged with the current date and time formatted with
`elnode-log-buffer-datetime-format'."
  (let ((name (or filename (buffer-file-name (get-buffer buffer-or-name)))))
    (with-current-buffer (get-buffer-create buffer-or-name)
      (let ((buffer-read-only nil))
        (unless (assq
                 'elnode-log-buffer-position-written
                 (buffer-local-variables))
          (make-local-variable 'elnode-log-buffer-position-written)
          (setq elnode-log-buffer-position-written (make-marker))
          (set-marker elnode-log-buffer-position-written (point-min)))
        ;; To test this stuff we could rip these functions out into
        ;; separate pieces?
        (save-excursion
          (goto-char (point-max))
          (insert
           (format
            "%s: %s\n"
            (format-time-string elnode-log-buffer-datetime-format)
            text))
          ;; Save the file if we have a filename
          (when name
            (if (not (file-exists-p (file-name-directory name)))
                (make-directory (file-name-directory name) t))
            ;; could be switched to write-region - probably better
            (append-to-file elnode-log-buffer-position-written (point-max) name)
            (set-marker elnode-log-buffer-position-written (point-max)))
          ;; Truncate the file if it's grown too large
          (goto-char (point-max))
          (forward-line (- elnode-log-buffer-max-size))
          (beginning-of-line)
          (delete-region (point-min) (point)))))))

(defcustom elnode-error-log-to-messages t
  "Whether to send elnode logging through the messaging system."
  :group 'elnode
  :type '(boolean))

(defvar elnode-server-error-log "*elnode-server-error*"
  "The buffer where error log messages are sent.")

(defvar elnode--do-error-logging t
  "Allows tests to turn off error logging.")

(defvar elnode--http-send-string-debug nil
  "Whether to do error logging in `elnode-http-send-string'.

That is very high logging, probably a bad idea for anyone but an
elnode developer.")

(defun elnode--get-error-log-buffer ()
  "Returns the buffer for the error-log."
  (get-buffer-create elnode-server-error-log))

(defmacro elnode-error (msg &rest args)
  "Log MSG with ARGS as an error.

This function is available for handlers to call.  It is also used
by elnode iteslf.

There is only one error log, in the future there may be more."
  `(when elnode--do-error-logging
     (let ((filename (elnode--log-filename "elnode-error"))
           (fmtmsg (format ,msg ,@args)))
       (elnode-log-buffer-log
        fmtmsg
        (elnode--get-error-log-buffer)
        filename)
       (when elnode-error-log-to-messages
         (message "elnode: %s" fmtmsg)))))

(defconst elnode-msg-levels (list :debug :info :status :warning)
  "Levels of message `elnode-msg' uses.")

(defmacro elnode--posq (element lst)
  "Return the index in the LST of ELEMENT."
  (let ((elv (make-symbol "el")))
    `(let ((,elv ,element))
       (catch :escape
         (let ((i 0))
           (dolist (e ,lst)
             (when (eq e ,elv)
               (throw :escape i))
             (setq i (+ i 1)))
           nil)))))

(defmacro elnode-msg (level msg &rest args)
  "Log MSG to the error console with a particular LEVEL.

LEVEL is compared to `elnode--do-error-logging'."
  (declare (indent 2))
  `(when (or (eq t elnode--do-error-logging)
             (>= (elnode--posq ,level elnode-msg-levels)
                 (elnode--posq
                  (or elnode--do-error-logging (car elnode-msg-levels))
                  elnode-msg-levels)))
     (elnode-error ,msg ,@args)))

(defun elnode--log-filename (logname)
  "Turn LOGNAME into a filename.

`elnode-log-files-directory' is used as the container for log files.

This function mainly exists to make testing easier."
  (when elnode-log-files-directory
    (expand-file-name
     (format "%s/%s"
             elnode-log-files-directory
             logname))))

(defvar elnode-log-access-format-path-width 20
  "How to truncate the path in the access log.")

(defun elnode-log-access-format-func (httpcon)
  "Standard access log format function."
  (format
   (concat
    "%s % 8d %s % "
    (number-to-string elnode-log-access-format-path-width)
    "s %s")
   (elnode/con-get httpcon :elnode-httpresponse-status)
   (or (elnode/con-get httpcon :elnode-bytes-written) 0)
   (elnode-http-method httpcon)
   (elnode-http-pathinfo httpcon)
   (format-time-string ""
    (time-subtract (current-time)
                   (elnode/con-get httpcon :elnode-http-started)))))

(defcustom elnode-log-access-default-formatter-function
  'elnode-log-access-format-func
  "The default access log formatter function.

This is used when there is no specific logger function for a
log-name."
  :group 'elnode
  :type 'function)

(defcustom elnode-log-access-alist nil
  "An association list of access log format functions for log names.

An access log format function receives the http connection and
should return a log line to be entered in the log buffer.

These override the default log formatter."
  :group 'elnode
  :type '(alist
          :key-type string
          :value-type function))

(defun elnode-log-access (logname httpcon)
  "Log the HTTP access in buffer LOGNAME.

This function is available for handlers to call.  It is also used
by elnode iteslf."
  (let* ((elnode-log-buffer-datetime-format "%Y-%m-%d-%H:%M:%S")
         (buffer-name (format "*%s-elnode-access*" logname))
         (filename (elnode--log-filename logname))
         (formatter
          (or
           (kva logname elnode-log-access-alist)
           elnode-log-access-default-formatter-function))
         (formatted
          (when formatter
            (funcall formatter httpcon))))
    (elnode-log-buffer-log formatted buffer-name filename)))


;; Defer stuff

(progn
  ;; Sets up the elnode defer signal
  (put 'elnode-defer
       'error-conditions
       '(error elnode elnode-defer))
  (put 'elnode-defer
       'error-message
       "Elnode handler processing defered"))

(defvar elnode--deferred '()
  "List of deferred pairs: (socket . handler).")

(defun elnode-defer-now (handler)
  "The function you call to defer processing of the current socket.

Pass in the current HANDLER.

FIXME: We could capture the current handler somehow? I think the
point is that whatever signals elnode-defer should be getting
control back when the deferred is re-processed."
  (signal 'elnode-defer handler))

(defmacro elnode-defer-until (guard &rest body)
  "Test GUARD and defer if it fails and BODY if it doesn't.

`httpcon' is captured in this macro which means the macro can
only be expanded where there is an inscope `httpcon'.

Inside the macro the symbol `elnode-defer-guard-it' is bound to
the value of the GUARD."
  (declare (indent 1))
  (let ((bv (make-symbol "bv"))
        (gv (make-symbol "gv"))
        (fv (make-symbol "fv")))
    `(let* ((,gv (lambda () ,guard))
            (elnode-defer-guard-it (funcall ,gv))
            (,bv (lambda (httpcon) ,@body))
            (,fv ; a y-combinator!
             (lambda (httpcon proc)
               (setq elnode-defer-guard-it (funcall ,gv))
               (if elnode-defer-guard-it
                   (funcall ,bv httpcon)
                   ;; the test failed we should defer again
                   (elnode-defer-now
                    (lambda (http-con)
                      (funcall proc http-con proc)))))))
       (if elnode-defer-guard-it
           (funcall ,bv httpcon)
           ;; The test failed, we should defer.
           (elnode-defer-now
            (lambda (httpcon) ; apply the y-combinator
              (funcall ,fv httpcon ,fv)))))))

(defun elnode--deferred-add (httpcon handler)
  "Add the specified HTTPCON/HANDLER pair to the deferred list."
  (elnode-msg :info "deferred-add: adding a defer %s for %s" handler httpcon)
  (push (cons httpcon handler) elnode--deferred))

(defun elnode--deferred-process-open (httpcon handler)
  "Process the HANDLER with the known open HTTPCON."
  ;; (elnode-error "defer - just before calling the handler %s" handler)
  (funcall handler httpcon))


;; Log levels
(defconst elnode-log-debug 0)
(defconst elnode-log-info 1)
(defconst elnode-log-warning 2)
(defconst elnode-log-critical 3)

(defvar elnode-defer-processor-log-level elnode-log-critical
  "Log level of the defer processor.")

(defun elnode--deferred-log (level msg &rest args)
  "Special log for elnode-deferreds"
  (when (>= level elnode-defer-processor-log-level)
    (elnode-msg :info (format "elnode-deferred-processor %s %s" msg args))))

(defvar elnode-defer-failure-hook nil
  "Hook called when a deferred socket fails.

The hook function is called with the http connection and the
failure state which either the symbol `closed' or the symbol
`failed'.")

(defconst elnode--debug-with-backtraces nil
  "Feature switch to include backtrace debugging support.")

(defmacro elnode/case (expr &rest clauses)
  "A better `case' implementation."
  (declare (indent 1)(debug (form &rest (sexp body))))
  (let* ((backwards (reverse clauses))
         (last-clause (car backwards))
         (other-clauses (cdr backwards))
         (else-clause (when (eq t (car last-clause)) last-clause)))
    `(catch :escapesym
       (let ((value (progn ,expr)))
         ,@(let (collected)
                (dolist (c (if else-clause other-clauses clauses) collected)
                  (setq collected
                        (cons `(when (eq ,(car c) value)
                                 (throw :escapesym (progn ,@(cdr c))))
                              collected))))
         ,(if else-clause `(throw :escapesym ,@(cdr else-clause)))))))

(defun elnode--deferred-processor ()
  "Process the deferred queue."
  (let ((run (random 5000)) ; use this to disambiguate runs in the logs
        (new-deferred (list)))
    (elnode--deferred-log elnode-log-info "start")
    (loop for pair in elnode--deferred
       do
         (let ((httpcon (car pair))
               (handler (cdr pair)))
           (elnode/case (process-status httpcon)
             ('open
              (elnode--deferred-log elnode-log-info
                                    "open %s %s" httpcon handler)
              (condition-case signal-value
                  (elnode--deferred-process-open httpcon handler)
                ('elnode-defer
                 (push
                  (cons httpcon (cdr signal-value))
                  new-deferred))
                (error
                 (elnode--deferred-log
                  elnode-log-critical
                  "error %s - %s %S" httpcon signal-value
                  (if elnode--debug-with-backtraces
                      debugger-previous-backtrace
                      "")))))
             ('closed
              (elnode--deferred-log elnode-log-info
                                    "closed %s %s" httpcon handler)
              ;; Call any hook function for defer closes
              (loop for hook-func in elnode-defer-failure-hook
                 do
                   (funcall hook-func httpcon 'closed)))
             ('failed
              (elnode--deferred-log
               elnode-log-info "failed %s %s" httpcon handler)
              ;; Call any hook function for defer failures
              (loop for hook-func in elnode-defer-failure-hook
                 do
                   (funcall hook-func httpcon 'failed)))
             ;; Not sure how to do connect... same as open?
             ;; ... or just put it back?
             ('connect
              (push
               (cons httpcon handler)
               new-deferred)))))
    (elnode--deferred-log elnode-log-info "complete")
    ;; Set the correct queue
    (setq elnode--deferred new-deferred)))

(defun elnode-deferred-queue-process ()
  (interactive)
  (elnode--deferred-processor))

(defvar elnode-defer-on nil
  "Whether to do deferring or not.")

(defvar elnode--defer-timer nil
  "The timer used by the elnode defer processing.

This is initialized by `elnode--init-deferring'.")

(defun elnode--init-deferring ()
  "Initialize elnode defer processing.

Necessary for running comet apps."
  (setq elnode--defer-timer
        (run-at-time "2 sec" 2 'elnode--deferred-processor)))

(defun elnode-deferred-queue-start ()
  "Start the deferred queue, unless it's running."
  (interactive)
  (unless elnode-defer-on
    (setq elnode-defer-on t))
  (unless elnode--defer-timer
    (elnode--init-deferring)))

(defun elnode-deferred-queue-stop ()
  "Stop any running deferred queue processor."
  (interactive)
  (when elnode--defer-timer
    (cancel-timer elnode--defer-timer)
    (setq elnode--defer-timer nil)))

;;; Basic response mangling

(defcustom elnode-default-response-table
  '((201 . "Created")
    (301 . "Moved")
    (302 . "Found")
    (304 . "Not modified")
    (400 . "Bad request")
    (404 . "Not found")
    (500 . "Server error")
    (t . "Ok"))
  "The status code -> default message mappings.

When Elnode sends a default response these are the text used.

Alter this if you want to change the messages that Elnode sends
with the following functions:

 'elnode-send-400'
 'elnode-send-404'
 'elnode-send-500'

The function `elnode-send-status' also uses these."
  :group 'elnode
  :type '(alist :key-type integer
                :value-type string))

(defconst elnode--default-response-groups
  '((1 . "Informing you of something.")
    (2 . "Ok.")
    (3 . "")
    (4 . "Bad.")
    (5 . "Error."))
  "Response codes for error code / 100.

These are designed to be used when a specific code is not
available.")

(defun elnode--format-response (status &optional msg)
  "Format the STATUS and optionally MESSAGE as an HTML return."
  (format "<h1>%s</h1>%s\r\n"
          (cdr (or (assoc status elnode-default-response-table)
                   (assoc (/ status 100) elnode--default-response-groups)
                   (assoc t elnode-default-response-table)))
          (if msg (format "<p>%s</p>" msg) "")))


;; Main control functions

(defun elnode--http-parse-header (buffer start &optional non-header)
  "Parse a header from the BUFFER at point START.

The initial header may be parsed with this or if NON-HEADER is
sent then another header, such as a multipart header, may be read.

If the complete header has not been read then we throw to
`elnode-parse-http' with either `header' or `non-header'.

We return a list of the leader, which is the first line of the
header (which is not the header) followed by an alist of
headers."
  (with-current-buffer buffer
    (let ((hdrend (re-search-forward "\r\n\r\n" nil 't)))
      (when (not hdrend)
        (throw 'elnode-parse-http (or (and non-header 'non-header) 'header)))
      (let* ((lines
              (split-string
               (buffer-substring start hdrend)
               "\r\n"
               't))
             (status (car lines)) ;; the first line is the status line
             (header (cdr lines)) ;; the rest of the lines are the header
             (header-alist-strings
              (mapcar
               (lambda (hdrline)
                 (when (string-match
                        "\\([A-Za-z0-9_-]+\\):[ ]*\\(.*\\)"
                        hdrline)
                   (cons
                    (downcase (match-string 1 hdrline))
                    (match-string 2 hdrline))))
               header)))
        (list status header-alist-strings)))))


(defun elnode--http-parse (process)
  "Parse the HTTP header for the PROCESS.

If the request is not fully complete (if the header has not
arrived yet or we don't have all the content-length yet for
example) this can throw `elnode-parse-http'.  The thing being
waited for is indicated.

Important side effects of this function are to add certain
process properties to the HTTP connection.  These are the result
of successful parsing."
  ;; FIXME - we don't need to do this - we should check for
  ;; header-parsed and avoid it we we can
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-min))
      (destructuring-bind (leader alist-strings)
          (elnode--http-parse-header (current-buffer) (point-min))
        (let* ((hdrend (point))
               (alist-syms
                (kvalist-keys->symbols alist-strings :first-fn 'downcase))
               (content-len (assq 'content-length alist-syms)))
          ;; Check the content if we have it.
          (when content-len
            (let* ((available-content (- (point-max) hdrend)))
              (when (> (string-to-number (cdr content-len))
                       available-content)
                (throw 'elnode-parse-http 'content))))
          (elnode/con-put process
            :elnode-header-end hdrend
            :elnode-http-status leader
            :elnode-http-header-syms alist-syms
            :elnode-http-header alist-strings)))))
  ;; Return a symbol to indicate done-ness
  'done)

(defun elnode--http-make-hdr (method resource &rest headers)
    "Convenience function to make an HTTP header.

METHOD is the method to use.  RESOURCE is the path to use.
HEADERS should be pairs of strings indicating the header values:

 (elnode--http-make-hdr 'get \"/\" '(host . \"localhost\"))

Where symbols are encountered they are turned into strings.
Inside headers they are capitalized.

A header pair with the key `body' can be used to make a content body:

 (elnode--http-make-hdr 'get \"/\" '(body . \"some text\"))
 =>
 GET / HTTP/1.1

 some text

No other transformations are done on the body, no content type
added or content length computed."
    (let (body)
      (noflet ((header-name (hdr)
               (if (symbolp (car hdr))
                   (symbol-name (car hdr))
                   (car hdr))))
        (format
         "%s %s HTTP/1.1\r\n%s\r\n%s"
         (upcase (if (symbolp method) (symbol-name method) method))
         resource
         (loop for header in headers
            if (equal (header-name header) "body")
            do (setq body (cdr header))
            else
            concat (format
                    "%s: %s\r\n"
                    (capitalize (header-name header))
                    (cdr header)))
         ;; If we have a body then add that as well
         (or body "")))))

(defun elnode/get-or-make-con-buffer (httpcon)
  (or
   (process-buffer httpcon)
   (let* ((port (cadr (process-contact httpcon)))
          (buf (get-buffer-create (format " *elnode-request-%s*" port))))
     (set-process-buffer httpcon buf)
     (process-buffer httpcon))))

(defsubst elnode--call (handler con)
  (funcall handler con))

(defun elnode--filter (process data)
  "Filtering DATA sent from the client PROCESS..

This does the work of finding and calling the user HTTP
connection handler for the request on PROCESS.

A buffer for the HTTP connection is created, uniquified by the
port number of the connection."
  (with-current-buffer (elnode/get-or-make-con-buffer process)
    (insert data)
    (elnode/case (catch 'elnode-parse-http (elnode--http-parse process))
      ('header (elnode-msg :info "filter: partial header data received"))
      ('content (elnode-msg :info "filter: partial header data received"))
      ('done
       (save-excursion
         (goto-char (elnode/con-get process :elnode-header-end))
         (let ((handler (elnode/get-server-prop process :elnode-http-handler)))
           (unwind-protect
                (condition-case signal-value 
                    (funcall handler process)
                  ('elnode-defer ; see elnode-defer-now
                   (elnode-msg :info "filter: defer caught on %s" process)
                   ;; Check the timer, this is probably spurious but useful "for now"
                   (unless elnode-defer-on
                     (elnode-msg :info "filter: no defer timer for %s" process))
                   (elnode/case (elnode/get-server-prop process :elnode-defer-mode)
                     (:managed
                      (elnode/con-put process :elnode-deferred t)
                      ;; the cdr of the sig value is the func
                      (elnode--deferred-add process (cdr signal-value)))
                     (:immediate
                      (elnode-msg :info "filter: immediate defer on %s" process)
                      (funcall (cdr signal-value) process))))
                  ('t
                   (unless (or (elnode/con-get process :elnode-child-process)
                               (elnode/con-get process :elnode-http-started))
                     (elnode-msg :info "filter: default handling %S" signal-value)
                     (process-send-string process (elnode--format-response 500)))))
             (if (and (not (or 
                            (elnode/con-get process :elnode-http-started)
                            (elnode/con-get process :elnode-child-process)))
                      (not (elnode/con-get process :elnode-deferred)))
                 (process-send-string process (elnode--format-response 500))
                 ;; Else
                 (when (elnode/con-get process :elnode-finished)
                   (unwind-protect
                        (progn
                          (delete-process process)
                          (kill-buffer (process-buffer process)))
                     (unless (eq 'closed (process-status process))
                       (elnode-msg :warning "elnode--filter failed at the end"))))))))))))

(defun elnode--ip-addr->string (ip-addr)
  "Turn a vector IP-ADDR into a string form.

The vector form is produced by `process-contact' and includes the
port number."
  (destructuring-bind (a b c d port)
      (mapcar 'identity ip-addr)
    (format "%s.%s.%s.%s:%s" a b c d port)))

(defun elnode-get-remote-ipaddr (httpcon)
  "Return the remote IP address from the HTTPCON.

Returned as a dotted ip address followed by a colon separated
port number.  For example: \"127.0.0.1:8080\"."
  (let* ((remote (plist-get
                  (process-contact httpcon t)
                  :remote)))
    (elnode--ip-addr->string remote)))

(defun elnode-server-info (httpcon)
  "Returns a string adress of the server host and port for HTTPCON.

For example: \"127.0.0.1:8000\" - localhost on port 8000."
  (elnode--ip-addr->string
   (plist-get
    (process-contact (elnode/con-get httpcon :server) t)
    :local)))


;;; Parsing

(defun elnode--alist-to-query (alist)
  "Turn an alist into a formdata/query string."
  (noflet ((web--key-value-encode (key value)
             "Encode a KEY and VALUE for url encoding."
             (cond
               ((or
                 (numberp value)
                 (stringp value))
                (format
                 "%s=%s"
                 (url-hexify-string (format "%s" key))
                 (url-hexify-string (format "%s" value))))
               (t
                (format "%s" (url-hexify-string (format "%s" key))))))
           (web--to-query-string (object)
             "Convert OBJECT (a hash-table or alist) to an HTTP query string."
             ;; Stolen from web
             (mapconcat
              (lambda (pair)
                (web--key-value-encode (car pair) (cdr pair)))
              (cond
                ((hash-table-p object)
                 (let (result)
                   (maphash
                    (lambda (key value)
                    (setq result (append (list (cons key value)) result)))
                    object)
                   (reverse result)))
                ((listp object)
                 object))
              "&")))
    (web--to-query-string alist)))

(defun elnode--make-test-call (path method parameters headers)
  "Construct the HTTP request for a test call.

This should probably be merged with the stuff in the `web'
module."
  (let* ((query
          (if (and parameters (equal method "GET"))
              (format
               "?%s"
               (elnode--alist-to-query parameters))
              ""))
         (http-path
          (if (equal query "")
              path
              (format "%s%s" path query)))
         (http-body
          (if (equal method "GET")
              nil
              (let ((param-data (elnode--alist-to-query parameters)))
                (setq headers
                      (append
                       (list
                        (cons "Content-Type"
                              "application/x-www-form-urlencoded")
                        (cons "Content-Length"
                              (format "%d" (length param-data))))
                       headers))
                param-data))))
    (apply
     'elnode--http-make-hdr
     `(,method
       ,http-path
       ,@headers
       (body . ,http-body)))))

(defun elnode--response-header-to-cookie-store (response)
  "Add Set-Cookie headers from RESPONSE to the cookie store."
  (let ((cookie-set (assoc "Set-Cookie" response)))
    (when cookie-set
      (let* ((cookie-value (car (split-string (cdr cookie-set) ";"))))
        (apply
         'puthash
         (append
          (split-string cookie-value "=")
          (list elnode--cookie-store))))))
  elnode--cookie-store)

(defun elnode--cookie-store-to-header-value ()
  "Turn the current cookie store into a header.

The cookies in the header are sorted alphabetically - makes
testing easier."
  (let ((cookie-value
         (mapconcat
          (lambda (cookie)
            (format "%s=%s" (car cookie)
                    (url-hexify-string (cdr cookie))))
          (kvalist-sort
           (kvhash->alist elnode--cookie-store)
           'string-lessp)
          "; ")))
    (unless (equal "" cookie-value)
      cookie-value)))

(defun* elnode-test-call (path
                          &key
                          (method "GET")
                          (parameters '())
                          (headers '()))
  "Fake a call to elnode with the PATH.

In addition you can specify some extra HTTP stuff:

 :method  one of GET, POST, DELETE, etc...
 :parameters POST parameters, will be turned into a POST body
 :headers any specific headers you require, you may override
   test-call headers.

For example:

 (elnode-test-call \"/wiki/test\")

or:

 (elnode-test-call \"/wiki/test\"
                   :method \"POST\"
                   :parameters '((\"a\" . 10)))

For header and parameter names, strings MUST be used currently.

During the test the variable `elnode-webserver-visit-file' is set
to `t' to ensure that Elnode does not pass fake HTTP connections
to external processes."
  (let ((fakir-mock-process-require-specified-buffer t))
    (fakir-mock-process :httpcon ()
      (let ((req (elnode--make-test-call
                  path method parameters
                  (append
                   headers
                   (let ((cookies (elnode--cookie-store-to-header-value)))
                     (when cookies
                       (list (cons "Cookie" cookies)))))))
            (http-con :httpcon)
            (the-end nil)
            (elnode-webserver-visit-file t))
        (noflet ((process-send-eof (proc) (setq the-end 't))
                 (process-status (proc) (if the-end 'closed 'open))
                 (elnode/get-server-prop (proc prop)
                   (elnode/case prop
                     (:elnode-defer-mode nil)
                     (t (funcall this-fn proc prop))))
                 ;; Do nothing - we want the test proc
                 (delete-process (proc))
                 ;; Again, do nothing, we want this buffer
                 (kill-buffer (buffer) t))
          ;; FIXME - we should unwind protect this?
          (elnode--filter http-con req)
          ;; Now we sleep till the-end is true
          (while (not the-end) (sit-for 0.1))
          (when the-end
            (elnode--response-header-to-cookie-store
             (elnode/con-get http-con :elnode-httpresponse-header))
            ;; Could we add to the cookie store here?
            (list
             :result-string
             (with-current-buffer (fakir-get-output-buffer)
               (buffer-substring-no-properties (point-min) (point-max)))
             :buffer (process-buffer http-con)
             ;; These properties are set by elnode-http-start
             :status (elnode/con-get http-con :elnode-httpresponse-status)
             :header (elnode/con-get http-con :elnode-httpresponse-header))))))))


(defvar elnode-handler-history '()
  "The history of handlers bound to servers.")

(defvar elnode-port-history '()
  "The history of ports that servers are started on.")

(defvar elnode-host-history '()
  "The history of hosts that servers are started on.")

(defun elnode-ports ()
  "List of all ports currently in use by elnode."
  (mapcar 'car elnode-server-socket))

(defun elnode/proc-log (server-proc client-proc msg)
  (set-process-plist client-proc (list (make-hash-table :test 'eq)))
  (elnode/con-put client-proc :server server-proc))

(defun elnode/make-service (host port service-mappings request-handler defer-mode)
  "Make an actual server TCP or Unix PORT.

If PORT is a number then a TCP port is made on the specified HOST
on the PORT.

If PORT is a string a Unix socket is made in \"/tmp/\" and HOST
is ignored."
  (let* ((name (format "*elnode-webserver-%s:%s*" host port))
         (an-buf (get-buffer-create name))
         (unix-sock-file-name (unless (numberp port) (concat "/tmp/" port)))
         (proc-args
          (list
           :name name
           :buffer an-buf
           :server (if (numberp port) 300 't)
           :nowait 't
           :host (cond
                   ((not (numberp port)) nil)
                   ((equal host "localhost") 'local)
                   ((equal host "*") nil)
                   (t host))
           :coding '(raw-text-unix . raw-text-unix)
           :family (if (numberp port) 'ipv4 'local)
           :service (if (numberp port) port unix-sock-file-name)
           :filter 'elnode--filter
           ;;:sentinel 'elnode--sentinel
           :log 'elnode/proc-log))
         (proc (apply 'make-network-process proc-args)))
    (elnode/con-put proc
      :elnode-service-map service-mappings
      :elnode-http-handler request-handler
      :elnode-defer-mode defer-mode)
    proc))

;;;###autoload
(defun* elnode-start (request-handler
                      &key
                      port
                      (host "localhost")
                      (defer-mode :managed)
                      service-mappings)
  "Start a server using REQUEST-HANDLER.

REQUEST-HANDLER will handle requests on PORT on HOST (which is
'localhost' by default).

REQUEST-HANDLER is a function which is called with the request.
The function is called with one argument, which is the
http-connection.

You can use functions such as `elnode-http-start' and
`elnode-http-send-body' to send the http response.

Example:

  (defun nic-server (httpcon)
    (elnode-http-start httpcon 200 '(\"Content-Type\" . \"text/html\"))
    (elnode-http-return httpcon \"<html><b>BIG!</b></html>\"))
  (elnode-start 'nic-server)

Now visit http://127.0.0.1:8000

If PORT is non-nil, then run server on PORT, otherwise default to
8000.

If HOST is non-nil, then run the server on the specified local IP
address, otherwise use localhost.  A few names are predefined:

  \"localhost\" is 127.0.0.1
  \"*\" is 0.0.0.0

Additionally, you may specifiy an IP address, e.g \"1.2.3.4\"

Note that although HOST may be specified, elnode does not
disambiguate on running servers by HOST.  So you cannot start two
elnode servers on the same port on different hosts.

DEFER-MODE may be used to control how deferred handlers are
managed for this server.

SERVICE-MAPPINGS is an alist of service resource symbols mapped
to integer port numbers.  This can be supplied to elnode-start to
allow it to map service resources defined by handlers to
different TCP ports and therefore different Emacs instances.

The list of SERVICE-MAPPINGS is also used to start ancilliary
port servers.  Ancilliary port servers should be automatically
stopped when the main server is stopped."
  (interactive
   (let ((handler (completing-read "Handler function: "
                                   obarray 'fboundp t nil nil))
         (port (read-number "Port: " 8000))
         (host (read-string "Host: " "localhost" 'elnode-host-history)))
     (list (intern handler) :port port :host host)))
  (let ((port (or port 8000))
        (host (or host "localhost")))
    (unless (assoc port elnode-server-socket)
      ;; Add a new server socket to the list
      (setq elnode-server-socket
            (cons
             (cons port
                   (let ((buf (get-buffer-create "*elnode-webserver*"))
                         (ancilliarys
                          (loop for (resource . port) in service-mappings
                             collect
                               (elnode/make-service
                                host port service-mappings
                                request-handler defer-mode)))
                         (main (elnode/make-service
                                host port service-mappings
                                request-handler defer-mode)))
                     ;; Add the link between the main and the ancilliarys
                     (elnode/con-put main :elnode-ancilliarys ancilliarys)
                     main))
             elnode-server-socket)))))

;; TODO: make this take an argument for the
(defun elnode-stop (port)
  "Stop the elnode server attached to PORT."
  (interactive
   (let ((prt
          (string-to-number
           (completing-read
            "Port: "
            (mapcar (lambda (n) (format "%s" n))
                    (elnode-ports))))))
     (list prt)))
  (let* ((server
          (or (assoc port elnode-server-socket)
              (assoc (format "%d" port) elnode-server-socket)))
         (port-to-kill (car-safe server)))
    (when server
      (message "deleting server process")
      (loop for ancilliary
         in (elnode/con-get (cdr server) :elnode-ancilliarys)
         do (delete-process ancilliary))
      ;; Now the main one
      (delete-process (cdr server))
      (setq elnode-server-socket
            ;; remove-if
            (let ((test (lambda (elem)
                          (equal (car elem) port-to-kill)))
                  (l elnode-server-socket)
                  result)
              (while (car l)
                (let ((p (pop l))
                      (r (cdr l)))
                  (if (not (funcall test p))
                      (setq result (cons p result)))))
              result)))))

(defun elnode-find-free-service ()
  "Return a free (unused) TCP port.

The port is chosen randomly from the ephemeral ports. "
  (let (myserver
        (port 50000)) ; this should be ephemeral base
    (while
        (not
         (processp
          (condition-case sig
              (setq myserver
                    (make-network-process
                     :name "*test-proc*"
                     :server t
                     :nowait 't
                     :host 'local
                     :service port
                     :family 'ipv4))
            (file-error
             (if (equal
                  "Cannot bind server socket address already in use"
                  (mapconcat 'identity (cdr sig) " "))
                 (setq port (+ 50000 (random 5000)))))))))
    (delete-process myserver)
    port))

(defun elnode-list-buffers ()
  "List the current buffers being managed by Elnode."
  (interactive)
  (with-current-buffer (get-buffer-create "*elnode-buffers*")
    (erase-buffer)
    (mapc
     (lambda (b)
       (save-excursion
         (if (string-match " \\*elnode-.*" (buffer-name b))
             (insert (format "%s\n" b)))
       ))
     (sort (buffer-list)
           (lambda (a b)
             (string-lessp (buffer-name b) (buffer-name a))))))
  (display-buffer (get-buffer "*elnode-buffers*")))

(defun elnode-time-encode (time-str)
  "Basic TIME-STR to time encoding."
  (apply 'encode-time (parse-time-string time-str)))


;; HTTP API methods

(defun elnode--http-hdr (httpcon)
  "Return the header cons for the HTTPCON.

The status-line and the header alist."
  (cons
   (elnode/con-get httpcon :elnode-http-status)
   (elnode/con-get httpcon :elnode-http-header)))

(defun elnode-http-headers (httpcon)
  "Return the alist of headers from HTTPCON."
  (elnode/con-get httpcon :elnode-http-header))

(defun elnode-http-header (httpcon name &optional convert)
  "Get the header specified by NAME from the HTTPCON.

HEADER may be a string or a symbol.  If NAME is a symbol it is
case insensitve.

If optional CONVERT is specified it may specify a conversion,
currently supported conversions are:

 :time - to convert a time value properly"
  (let* ((key (if (symbolp name)
                  (intern (downcase (symbol-name name)))
                name))
         (hdr (if (symbolp key)
                  (elnode/con-get httpcon :elnode-http-header-syms)
                  (elnode/con-get httpcon :elnode-http-header)))
         (val (cdr (assoc (if (stringp key) (downcase key) key) hdr))))
    (elnode/case convert
      (:time
       (when val
         (elnode-time-encode val)))
      (t
       val))))

(defun* elnode-http-host (httpcon &key split just-host)
  "Return the HTTP `host' name header.

With SPLIT return a list of the hostname and any port part (the
port part might be empty if not specifically specified).  With
JUST-HOST return just the host-name part, dropping any port entirely."
  (let ((host (elnode-http-header httpcon "Host")))
    (cond
      (split
       (string-match "\\([^:]+\\)\\(:\\([0-9]+\\)\\)*" host)
       (list (match-string-no-properties 1 host)
             (match-string-no-properties 3 host)))
      (just-host
       (string-match "\\([^:]+\\)\\(:\\([0-9]+\\)\\)*" host)
       (match-string-no-properties 1 host))
      (t
       host))))

(defun elnode-http-cookies (httpcon)
  "Return the list of cookies attached to this HTTPCON.

The list of cookies is an alist."
  (or
   (elnode/con-get httpcon :elnode-http-cookie-list)
   (let* ((cookie-hdr (elnode-http-header httpcon 'Cookie))
          (lst (when cookie-hdr
                 (kvalist-sort
                  (mapcar
                   (lambda (pair)
                     (cons
                      (url-unhex-string (car pair))
                      (url-unhex-string (cdr pair))))
                   (url-parse-args cookie-hdr))
                  'string-lessp))))
     (elnode/con-put httpcon :elnode-http-cookie-list lst)
     lst)))

(defun elnode-http-cookie (httpcon name &optional cookie-key)
  "Return the cookie value for HTTPCON specified by NAME.

The cookie is a cons:

  name . value

If COOKIE-KEY is `t' then only the value is returned, else the
cons is returned."
  (let* ((cookie-list (elnode-http-cookies httpcon))
         (cookie (assoc-string name cookie-list)))
    (if cookie-key
        (cdr cookie)
        cookie)))

(defconst elnode--http-status-line-rx
  (rx (and (group-n 1 (or "GET" "HEAD" "POST" "DELETE" "PUT"))
           " "
           (group-n 2 (1+ (any "A-Za-z0-9+&=?./:;_-"))) ; FIXME - get this from the spec?
           " "
           "HTTP/"
           (group-n 3 (and "1." (1+ (any "0-9"))))))
  "The regex used to match the status line.")

(defun elnode--http-parse-status (httpcon &optional property)
  "Parse the status line of HTTPCON.

If PROPERTY is non-nil, then return that property."
  (let* ((http-line (elnode/con-get httpcon :elnode-http-status)))
    (save-match-data
      (when (and http-line
                 (string-match elnode--http-status-line-rx http-line))
        (elnode/con-put httpcon
          :elnode-http-method (match-string 1 http-line)
          :elnode-http-resource (match-string 2 http-line)
          :elnode-http-version (match-string 3 http-line)
          :elnode-http-parsed-time (current-time))
        (when property
          (elnode/con-lookup httpcon property))))))

(defun elnode--http-parse-resource (httpcon &optional property)
  "Convert the specified resource to a path and a query."
  (let ((resource
         (or
          (elnode/con-get httpcon :elnode-http-resource)
          (elnode--http-parse-status
           httpcon :elnode-http-resource))))
    (save-match-data
      (if (or
           ;; root pattern with 
           (string-match "^\\(/\\)\\(\\?.*\\)*$" resource)
           ;; /somepath or /somepath/somepath
           (string-match "^\\(/[^?]+\\)\\(\\?.*\\)*$" resource))
          (let ((path (url-unhex-string (match-string 1 resource))))
            (elnode/con-put httpcon :elnode-http-pathinfo path)
            (when (match-string 2 resource)
              (let ((query (match-string 2 resource)))
                (string-match "\\?\\(.+\\)" query)
                (if (match-string 1 query)
                    (elnode/con-put
                     httpcon
                     :elnode-http-query
                     (match-string 1 query))))))
          ;; Else it might be a more exotic path
          (elnode/con-put httpcon :elnode-http-pathinfo resource))))
  (when property
    (elnode/con-lookup httpcon property)))

(defun elnode-http-pathinfo (httpcon)
  "Get the PATHINFO of the request.

The PATHINFO is the CGI term for the part of the path that is not
the hostname or the query; the part that relates to the path."
   (or
    (elnode/con-get httpcon :elnode-http-pathinfo)
    (elnode--http-parse-resource httpcon :elnode-http-pathinfo)))

(defun elnode-http-query (httpcon)
  "Get the QUERY of the request."
  (or
   (elnode/con-get httpcon :elnode-http-query)
   (elnode--http-parse-resource httpcon :elnode-http-query)))

(defun elnode--http-param-part-decode (param-thing)
  "Decode an HTTP URL parameter part.

For example in:

 http://nic.ferrier.me.uk/blog/elnode/?p=10&a+c=20&d=x+y&z=this%20is%09me+and%20this

The following are param parts and the decoding that this function
will do:

 \"p\" ->  \"p\"

 \"10\" -> \"10\"

 \"a+c\" -> \"a c\" - an example of + encoding

 \"d\" -> \"d\"

 \"x+y\" -> \"x y\" - another example of + encoding, in a parameter name

 \"z\" -> \"z\"

 \"this%20is%09me+and%20this\" -> \"this is\tme and this\" -
 percent encoding and plus encoding"
  (url-unhex-string (replace-regexp-in-string "\\+" " " param-thing) 't))

(defun elnode--http-query-to-alist (query)
  "Crap parser for HTTP QUERY data.

Returns an association list."
  (--map
   (if (string-match "\\([^=]+\\)\\(=\\(.*\\)\\)*" it)
       (cons
        (elnode--http-param-part-decode (match-string 1 it))
        (if (match-string 2 it)
            (elnode--http-param-part-decode (match-string 3 it))
            nil)))
   (split-string query "&")))

(defalias 'elnode-http-query-to-alist 'elnode--http-query-to-alist
  "Elnode's HTTP query data parser.

Send a string and get back an association list.")

(defun elnode--http-form-encoded-to-alist (buffer)
  "Parse BUFFER containing a POST body and return an a-list.

BUFFER must be positioned correctly at the start of the POST
body.  The BUFFER must have a final dummy character added to
identify the end of the text.  Currently an ASCII NUL (0) is
used.

The returned alist contains text parameter names mapped to
decoded parameter values.  The decoding is as for
`elnode--http-param-part-decode'.

There is one experimental case which you must be careful of.
Image data parameters are identified specifically by Elnode in
order to parse them efficiently.  Such a parameter is of the
form:

  data:mime/type;base64,BASE64.DATA.

If a parameter like that is found it is returned with the data as
a string parameter value but the mime-type set on the string as a
property `:mime-type'.  This is an experimental feature and
subject to change."
  ;; This function is required because string-match won't parse very
  ;; large regexs, like those of submitted image files.
  (with-current-buffer buffer
    (narrow-to-region (point) (point-max))
    (goto-char (point-min))
    (let* ((positions-points
            (-flatten
             (append
              (list (point-min-marker))
              (--map
               (list it it)
               (let (collect expr)
                 (save-excursion
                   (save-match-data
                     (while
                         (setq expr
                               (and (re-search-forward "&" nil t)
                                    (point-marker)))
                       (setq collect (append collect (list expr))))
                     collect))))
              (list (point-max-marker)))))
           (positions (-partition 2 positions-points)))
      (save-excursion
        (save-match-data
          (--map
           (let ((start (car it))
                 (end (cadr it)))
             (goto-char start)
             (let ((param-name
                    (let ((found (re-search-forward "=" end t)))
                      (buffer-substring start (if found (1- found) end)))))
               (save-excursion
                 (while (re-search-forward "\\+" end t)
                   (replace-match " ")))
               (save-excursion
                 (while (re-search-forward "%[0-9a-f]\\{2\\}" end t)
                   (let ((md (save-match-data (url-unhex-string (match-string 0) t))))
                     (replace-match md t t))))
               (if (save-excursion (re-search-forward "data:\\([^;]+\\);base64," nil end))
                   (cons
                    param-name
                    (progn
                      (base64-decode-region (match-end 0) (1- end))
                      (propertize
                       (buffer-substring (match-end 0) (1- end))
                       :mime-type (match-string 1))))
                   ;; else it's just a param
                   (cons param-name (buffer-substring (point) (1- end))))))
           positions))))))

(defun elnode--alist-merge (a b &optional operator)
  "Merge two association lists non-destructively.

A is considered the priority (its elements go in first)."
  (if (not operator)
      (setq operator 'assq))
  (let* ((res '()))
    (let ((lst (append a b)))
      (while lst
        (let ((item (car-safe lst)))
          (setq lst (cdr-safe lst))
          (let* ((key (car item))
                 (aval (funcall operator key a))
                 (bval (funcall operator key b)))
            (if (not (funcall operator key res))
                (setq res (cons
                           (if (and aval bval)
                               ;; the item is in both lists
                               (cons (car item)
                                     (list (cdr aval) (cdr bval)))
                             item)
                           res))))))
        res)))

(defun elnode--http-mp-find-boundary (boundary)
  "Find the boundary string from point."
  (let ((boundary-rx
         (rx-to-string `(seq "\r\n--" ,boundary))))
    (save-match-data
      (when (re-search-forward boundary-rx nil t)
        (let ((mpt (match-beginning 0)))
          ;; Return status indicator and the start match point
          (list
           (if (save-excursion
                 (goto-char (line-beginning-position))
                 (looking-at (rx-to-string `(seq bol "--" ,boundary "--"))))
               :done :continue)
           (progn (goto-char mpt) mpt)))))))

(defun elnode--http-mp-decode (buffer header-end-pt boundary)
  "Decode a multipart/form-data upload with BOUNDARY in BUFFER."
  (with-current-buffer buffer
    (goto-char (- header-end-pt 2)) ; moves back over the \r\n
    (loop while (eq (car next-boundary) :continue)
       with next-boundary = (elnode--http-mp-find-boundary boundary)
       collect
         (destructuring-bind (leader alist)
             (elnode--http-parse-header (current-buffer) (point) t)
           (let* ((cde
                   (mail-header-parse-content-disposition
                    (kva "content-disposition" alist)))
                  (name (kva 'name (cdr cde)))
                  (filename (kva 'filename (cdr cde)))
                  (pt (point)))
             ;; Find the next end point
             (setq next-boundary
                   (elnode--http-mp-find-boundary boundary))
             (let* ((lbp (line-beginning-position))
                    (content (buffer-substring pt (cadr next-boundary)))
                    (content-data
                     (if (equal
                          "base64"
                          (downcase (or (kva "content-transfer-encoding" alist) "")))
                         (base64-decode-string content)
                         content))
                    (content-object
                     (cond
                       ((not filename) content-data)
                       (t (propertize content-data :elnode-filename filename)))))
               (cons name content-object)))))))

(defun elnode--http-post-mp-decode (httpcon parsed-content-type)
  "Decode the HTTP POST multipart thing on HTTPCON."
  (let ((boundary (kva 'boundary (cdr parsed-content-type)))
        (buf (process-buffer httpcon))
        (hdr-end-pt (elnode/con-get httpcon :elnode-header-end)))
    (elnode--http-mp-decode buf hdr-end-pt boundary)))

(defun elnode--http-post-body (httpcon)
  "Get the HTTP POST body."
  (with-current-buffer (process-buffer httpcon)
    ;; (buffer-substring (point-min) (point-max)) ;debug
    (buffer-substring
     ;; we might have to add 2 to this because of trailing \r\n
     (elnode/con-get httpcon :elnode-header-end)
     (point-max))))

(defun elnode--http-post-to-alist (httpcon)
  "Parse the POST body."
  ;; FIXME: this is ONLY a content length header parser -- it should
  ;; also cope with transfer encodings.
  (let* ((content-type (elnode-http-header httpcon 'content-type))
         (parsed-type
          (when content-type
            (mail-header-parse-content-type content-type))))
    (if (equal "multipart/form-data" (car parsed-type))
        (elnode--http-post-mp-decode httpcon parsed-type)
        ;; Else it's a non-multipart request
        (with-current-buffer (process-buffer httpcon)
          (goto-char (point-max))
          (insert "\0")
          (goto-char (elnode/con-get httpcon :elnode-header-end))
          (elnode--http-form-encoded-to-alist (current-buffer)))
        ;;(elnode--http-query-to-alist (elnode--http-post-body httpcon))
        )))

(defun elnode-http-params (httpcon &rest names)
  "Get an alist of the parameters in the request.

If the method is a GET then the parameters are from the url.  If
the method is a POST then the parameters may come from either the
url or the POST body or both:

 POST /path?a=b&x=y
 a=c

would result in:

 '((\"a\" \"b\" \"c\")(\"x\" . \"y\"))

If NAMES are specified it is a filter list of symbols or strings
which will be returned.

File upload with Multipart/form-data is supported by Elnode.
Uploaded files are present in the params the same as any other
param except for the fact that uploaded file params have a text
property :elnode-filename on them:

  (get-text-property 0 :elnode-filename
    (elnode-http-param httpcon \"myfile\")) => '/somefile.txt'

The value comes from the \"Content-Disposition\" header in the
multipart upload."
  (loop for pair in
       (or
        (elnode/con-get httpcon :elnode-http-params)
        (let ((query (elnode-http-query httpcon)))
          (let ((alist (if query
                           (elnode--http-query-to-alist query)
                           '())))
            (if (equal "POST" (elnode-http-method httpcon))
                ;; If we're a POST we have to merge the params
                (progn
                  (setq alist
                        (elnode--alist-merge
                         alist
                         (elnode--http-post-to-alist httpcon)
                         'assoc))
                  (elnode/con-put httpcon :elnode-http-params alist)
                  alist)
                ;; Else just return the query params
                (elnode/con-put httpcon :elnode-http-params alist)
                alist))))
     if (or (not names)
            (memq (intern (car pair)) names)
            (member (car pair) names))
     collect pair))

(defun elnode-http-param (httpcon name &optional default)
  "Get the parameter named NAME from the request.

If the parameter came from a file upload it has a text property
indicating the filename:

  (get-text-property 0 :elnode-filename
    (elnode-http-param httpcon \"myfile\")) => '/somefile.txt'

If the parameter is not present and DEFAULT is present then
return DEFAULT instead of `nil'."
  (let* ((params (elnode-http-params httpcon))
         (param-pair
          (assoc
           (if (symbolp name) (symbol-name name) name)
           params)))
    ;; Should we signal when we don't have a param?
    (if param-pair
        (cdr param-pair)
        default)))

(defun elnode-http-method (httpcon)
  "Get the HTTP request method (GET, PUT, etc...) as a string."
  (or
   (elnode/con-get httpcon :elnode-http-method)
   (elnode--http-parse-status httpcon :elnode-http-method)))

(defun elnode-http-version (httpcon)
  "Get the PATHINFO of the request."
  (or
   (elnode/con-get httpcon :elnode-http-version)
   (elnode--http-parse-status httpcon :elnode-http-version)))

(defun elnode-http-send-string (httpcon str)
  "Send STR to HTTPCON, doing chunked encoding."
  (elnode-msg :debug
      "elnode-http-send-string %s [[%s]]" httpcon (s-truncate 10 str))
  (let ((len (string-bytes str)))
    (elnode/con-put httpcon :elnode-bytes-written
                 (+ len (or (elnode/con-get httpcon :elnode-bytes-written) 0)))
    ;; FIXME Errors can happen here, because the socket goes away.. it
    ;; would be nice to trap them and report and then re-raise them.
    (if (eq (process-status httpcon) 'open)
        (condition-case err
            (process-send-string
             httpcon
             (format "%x\r\n%s\r\n" (length str) (or str "")))
          (error
           (elnode-msg :warning
               "elnode-http-send-string failed to send [%s] on %s (%s)"
             (length str) httpcon (process-status httpcon))))
        (elnode-msg :warning
            "elnode-http-send-string can't print [%s] because %s is %s"
          (length str) httpcon (process-status httpcon)))))

(defconst elnode-http-codes-alist
  (loop for p in '((200 . "Ok")
                   (201 . "Created")
                   (302 . "Moved")
                   (302 . "Found")
                   (304 . "Not Modified")
                   (400 . "Bad Request")
                   (401 . "Authenticate")
                   (404 . "Not Found")
                   (500 . "Server Error"))
     ;; add an alist entry with an integer key
     collect p
     ;; add an alist entry with a string key
     collect
       (cons (number-to-string (car p))
             (cdr p)))
  "HTTP codes with string keys and integer keys.")

(defun* elnode-http-cookie-make (name data &key expiry path)
  "Make a set-cookie header pair from NAME and DATA.

DATA should be a string to be used as the value of the cookie.

Other key values are standard cookie attributes.

Use this with `elnode-http-start' to make cookie headers:

 (elnode-http-start
    httpcon 200
    '(content-type . \"text/html\")
    (elnode-http-cookie-make \"pi\" 3.14579)
    (elnode-http-cookie-make \"e\" 1.59
       :expiry \"Mon, Feb 27 2012 22:10:21 GMT;\")

This will send two Set-Cookie headers setting the cookies 'pi'
and 'e'.

The return value is a cons pair."
  (cons
   "Set-Cookie"
   (format "%s=%s;%s"
           name
           data
           (if (not (or expiry
                        path))
               ""
               (loop for p in `((expires . ,expiry)
                                (path . ,path))
                  if (cdr p)
                  concat
                    (format
                     " %s=%s;"
                     (capitalize (symbol-name (car p)))
                     (cdr p)))))))

(defun elnode-http-header-set (httpcon header &optional value)
  "Sets the HEADER for later processing.

HEADER may be a pair of `name' and `value' or it may just be a
String, or a Symbol in which case the VALUE must be specified.

If HEADER is a pair and VALUE is also specified then VALUE is
ignored.

When the HTTP response is started any set headers will be merged
with any requested headers and sent.

If the response has been started it is an error to try to set a
header.  This function will log the error and return `nil'.

See `elnode-http-start'."
  (if (elnode/con-get httpcon :elnode-http-started)
      (elnode-msg :warning "can't set header, HTTP already started on %s" httpcon)
      (let ((headers (elnode/con-get httpcon :elnode-headers-to-set)))
        (elnode/con-put
         httpcon
         :elnode-headers-to-set
         (append headers
                 (list (if (consp header)
                           header
                           (cons header value))))))))

(defun* elnode-http-cookie-set (httpcon name data &key expiry path)
  "Make a cookie and set it on the HTTPCON.

See `elnode-http-cookie-make' for details about cookie making."
  (let ((cookie-cons (elnode-http-cookie-make
                      name data :expiry expiry :path path)))
    (elnode-http-header-set httpcon (car cookie-cons) (cdr cookie-cons))))

(defun elnode--http-result-header (hdr-alist)
  "Turn the HDR-ALIST into a result header string.

The HDR-ALIST is an alist of symbol or string keys which are
header names, against values which should be strings."
  (let ((hdr-pairs
         (append
          (list (cons 'transfer-encoding "chunked"))
          hdr-alist)))
    (loop for p in hdr-pairs
       concat
         (format
          "%s: %s\r\n"
          (let ((hname (car p)))
            (capitalize
             (cond
               ((symbolp hname)
                (symbol-name hname))
               ((stringp hname)
                hname)
               (t
                (error "unsupported header type")))))
          (cdr p)))))

(defvar elnode-http-special-extra-headers nil
  "Set to a list of HTTP header conses to send with `elnode-http-start'.

For example:

 (let ((elnode-http-special-extra-headers
       '((\"Access-Control-Allow-Origin\" . \"*\")
         (\"Cache-control\" . \"none\"))))
   (elnode-http-start httpcon 200 '(Content-type \"text/html\")))

will send the combination of all the headers.

This is an alternative to use `elnode-header-set' on the
HTTPCON.")

(defun elnode-http-start (httpcon status &rest header)
  "Start the http response on the specified http connection.

HTTPCON is the HTTP connection being handled.

STATUS is the HTTP status, eg: 200 or 404; integers or strings
are acceptable types.

HEADER is a sequence of (`header-name' . `value') pairs.  See
`elnode-http-header-set' and `elnode-http-special-extra-headers'
for other ways to get headers onto the request.

For example:

 (elnode-http-start httpcon \"200\" '(\"Content-type\" . \"text/html\"))

The status and the header are also stored on the process as meta
data.  This is done mainly for testing infrastructure."
  (if (elnode/con-get httpcon :elnode-http-started)
      (elnode-msg :warning "elnode-http-start: HTTP already started on %s" httpcon)
      ;; Send the header
      (elnode-msg :debug "elnode-http-start: starting HTTP response on %s" httpcon)
      (let ((header-alist
             (append (elnode/con-get httpcon :elnode-headers-to-set)
                     elnode-http-special-extra-headers header))
            (status-code (if (stringp status)
                             (string-to-number status)
                             status)))
        ;; Store the meta data about the response.
        (elnode/con-put httpcon
          :elnode-httpresponse-status status-code
          :elnode-httpresponse-header header-alist)
        (process-send-string
         httpcon
         (format
          "HTTP/1.1 %d %s\r\n%s\r\n"
          status-code
          ;; The status text
          (kva status-code elnode-http-codes-alist)
          ;; The header
          (or
           (elnode--http-result-header header-alist)
           "\r\n")))
        (elnode/con-put httpcon :elnode-http-started (current-time)))))

(defun elnode--http-end (httpcon)
  "Marks the HTTPCON ended and does end of request things.

This makes access log file calls if the socket has a property
`:elnode-access-log-name'.  The property is taken to be the name
of a buffer."
  (elnode-msg :info "elnode--http-end ending socket %s" httpcon)
  (let ((access-log-name (elnode/con-get httpcon :elnode-access-log-name)))
    (when access-log-name
      (condition-case err
          (elnode-log-access access-log-name httpcon)
        (error
         (when nil
           (elnode-msg :warning
               "elnode--http-end: an error occurred processing the access log"))))))
  (when (eq 'open (process-status httpcon)) (process-send-eof httpcon))
  ;; Signal to elnode--filter that we're done
  (elnode/con-put httpcon :elnode-finished t))

(defun elnode-http-return (httpcon &optional data)
  "End the response on HTTPCON optionally sending DATA first.

HTTPCON is the http connection which must have had the headers
sent with `elnode-http-start'

DATA must be a string, it's just passed to `elnode-http-send'."
  (if (not (elnode/con-get httpcon :elnode-http-started))
      (elnode-msg :warning "elnode-http-return: HTTP not started")
      ;; We've got a valid HTTP connection, now send the end.
      (when data
        (elnode-http-send-string httpcon data))
      ;; Need to close the chunked encoding here
      (elnode-http-send-string httpcon "")
      (elnode--http-end httpcon)))

(defun elnode-send-html (httpcon html)
  "Simple send for HTML.

Use this for simple sending of a full HTML response:

 (defun my-handler (httpcon)
   (elnode-send-html httpcon \"<html><h1>Hello!</h1></html>\"))

The data is sent with content type: text/html."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return httpcon html))

(defun elnode-json-fix (data)
  "Fix JSON "
  (let ((json-to-send
         (noflet
             ((json-alist-p (list)
                "Proper check for ALIST."
                (while (consp list)
                  (setq list
                        (if (and
                             (consp (car list))
                             (not (consp (caar list)))
                             (not (vectorp (caar list))))
                            (cdr list)
                            'not-alist)))
                (null list)))
           (json-encode data)))) json-to-send))

(defun elnode-send-report (httpcon)
  "Send back an HTML report on the request.

This is often useful for debugging."
  (noflet ((alist->html (alist)
             (mapconcat
              (lambda (hdr-pair)
                (format
                 "%s %s"
                 (car hdr-pair)
                 (let ((v (cdr hdr-pair)))
                   (if (and v (not (equal v "")))
                       (format "%S" v) ""))))
              alist
              "\n")))
    (let* ((method (elnode-http-method httpcon))
           (paramters (alist->html
                       (or (elnode-http-params httpcon)
                           '(("None". "")))))
           (headers (alist->html (elnode-http-headers httpcon)))
           (page (s-lex-format "<html>
<meta charset=\"utf-8\"></meta>
<style>
body { font-family: sans-serif;}
td {
vertical-align: top;
}
</style>
<body>
<table>
<tr><td>method:</td><td>${method}</td></tr>
<tr><td>parameters:</td><td><pre>${paramters}</pre></td><tr>
<tr><td>headers:</td><td><pre>${headers}</pre></td><tr>
</table>
</body>
</html>")))
      (elnode-send-html httpcon page))))

(defun* elnode-send-json (httpcon data &key content-type jsonp)
  "Convert DATA to JSON and send to the HTTPCON with a 200 \"Ok\".

DATA is some lisp object.

If CONTENT-TYPE is specified then it is used as the HTTP Content
Type of the response.

If JSONP is specified the content is sent as a JSON-P response.
If the variable specifies a name for the JSON-P callback function
that that is used.  Alternately, if the JSONP parameter does not
specify a name, the parameter `callback' is looked up on the
HTTPCON and the value of that used.  If neither the JSONP
parameter, not the HTTP parameter `callback' is present that the
name \"callback\" is used."
  (let ((json-to-send (elnode-json-fix data)))
    (elnode-http-start
     httpcon 200
     `("Content-type" . ,(or content-type "application/json")))
    (elnode-http-return
     httpcon
     (if jsonp
         (format
          "%s(%s);"
          (or (when (stringp jsonp)
                jsonp)
              (elnode-http-param httpcon "callback")
              "callback")
          json-to-send)
         json-to-send))))

(defun elnode-send-status (httpcon status &optional msg)
  "A generic handler to send STATUS to HTTPCON.

Sends an HTTP response with STATUS to the HTTPCON.  An HTML body
is sent by looking up the STATUS in the `elnode-default-response'
table.

Optionally include MSG."
  (elnode-http-start httpcon status '("Content-type" . "text/html"))
  (elnode-http-return httpcon
                      (elnode--format-response status msg)))

(defun elnode-send-404 (httpcon &optional msg)
  "Sends a Not Found error to the HTTPCON.

Optionally include MSG."
  (elnode-send-status httpcon 404 msg))

(defun elnode-send-400 (httpcon &optional msg)
  "Sends a Bad Request error to the HTTPCON.

Optionally include MSG."
  (elnode-send-status httpcon 400 msg))

(defun elnode-send-500 (httpcon &optional msg)
  "Sends a Server Error to the HTTPCON.

Optionally include MSG."
  (elnode-send-status httpcon 500 msg))

(defun elnode-send-redirect (httpcon location &optional type)
  "Sends a redirect to LOCATION.

If TYPE is non-nil, use it as a status code.  Defaults to 302 -
permanent redirect."
  (let ((status-code (or type 302)))
    (elnode-http-start httpcon status-code `("Location" . ,location))
    (elnode-http-return
     httpcon
     (format "<h1>redirecting you to %s</h1>\r\n" location))))

(defun elnode-normalize-path (httpcon handler)
  "A decorator for HANDLER that normalizes paths to have a trailing slash.

This checks the HTTPCON path for a trailing slash and sends a 302
to the slash trailed url if there is none.

Otherwise it calls HANDLER."
  (let ((ends-in-slash-or-extension-regex ".*\\(/\\|.*\\.[^/]*\\)$")
        (path (elnode-http-pathinfo httpcon)))
    (if (not (save-match-data
               (string-match ends-in-slash-or-extension-regex
                             path)))
        (elnode-send-redirect
         httpcon
         (format "%s/" path))
      (funcall handler httpcon))))

(defun elnode--mapper-find-match-func (match-path match-pair)
  "Funtion to test MATCH-PATH against MATCH-PAIR."
  (let ((m (string-match (car match-pair) match-path)))
    (and m
         (numberp m)
         (>= m 0)
         match-pair)))

(defun elnode--mapper-find-mapping (match-path mapping-table)
  "Return the mapping that matches MATCH-PATH in MAPPING-TABLE."
  (loop for mapping in mapping-table
     if (elnode--mapper-find-match-func match-path mapping)
     return mapping))

(defun elnode--mapper-find (httpcon path mapping-table)
  "Try and find the PATH inside the MAPPING-TABLE.

This function exposes its `match-data' on the 'path' variable so
that you can access that in your handler with something like:

 (match-string 1 (elnode-http-pathinfo httpcon))

Returns the handler function that mapped, or `nil'.

This function also establishes the `:elnode-http-mapping'
property, adding it to the HTTPCON so it can be accessed from
inside your handler with `elnode-http-mapping'."
  ;; First find the mapping in the mapping table
  (let* ((pair (elnode--mapper-find-mapping path mapping-table))
         (func-item (and pair
                         (let* ((v (cdr pair)))
                           (or (and (atom v) v)
                               (if (functionp (car v))
                                   (car v)
                                   (when (functionp v) v)))))))
    ;; Now work out if we found one and what it was mapped to
    (when (or (functionp func-item)
              (functionp (and (symbolp func-item)
                              (symbol-value func-item))))
      ;; Make the match parts accessible
      (elnode/con-put
       httpcon
       :elnode-http-mapping
       (when (string-match (car pair) path)
         (loop for i from 0 to (- (/ (length (match-data path)) 2) 1)
               collect (match-string i path))))
      ;; Return the function
      func-item)))

(defun elnode--http-mapping-implementation (httpcon &optional part)
  "The actual implementation of `elnode-http-mapping.'

This is here so that you flet `elnode-http-mapping' and still get
at the real functionality."
  (if (eq part t)
      (length (elnode/con-get httpcon :elnode-http-mapping))
      ;; Else it's a specific part
      (elt
       (elnode/con-get httpcon :elnode-http-mapping)
       (if part part 0))))

(defun elnode-http-mapping (httpcon &optional part)
  "Return the match on the HTTPCON that resulted in the current handler.

With PART it returns a specific part of the match, by default
PART is 0.  If PART is specified as `t' then the count of parts
is returned.

This results only from a call via `elnode-dispatcher'.

It returns the string which matched your url-mapping, with the
match-data attached. So given the mapping:

 (\"/static/\\(.*\\)\" . my-handler)

and the request:

 /static/somedir/somefile.jpg

The following is true inside the handler:

 (equal \"/somedir/somefile.jpg\"
        (match-string 1 (elnode-http-mapping httpcon)))

The function `elnode-test-path' uses this facility to work out a
target path."
  (elnode--http-mapping-implementation httpcon part))

(defsubst elnode--strip-leading-slash (str)
  "Strip any leading slash from STR.

If there is no leading slash then just return STR."
  (if (and (stringp str)
           (> (length str) 0)
           (eq (elt str 0) ?/))
      (substring str 1)
      str))

(defun elnode-get-targetfile (httpcon docroot)
  "Get the targeted file from the HTTPCON.

Attempts to resolve the matched path of the HTTPCON against the
DOCROOT.  If that doesn't work then it attempts to use just the
pathinfo of the request.

The resulting file is NOT checked for existence or safety."
  (let* ((pathinfo (elnode-http-pathinfo httpcon))
         (path (elnode-http-mapping httpcon 1))
         (targetfile
          (elnode-join
           (expand-file-name docroot)
           (elnode--strip-leading-slash
            (or path pathinfo)))))
    targetfile))


;; We need to declare this before the dispatcher stuff, which uses it.
(defvar elnode--defined-authentication-schemes
  (make-hash-table :test 'equal)
  "The hash of defined authentication schemes.")

(defvar elnode--do-access-logging-on-dispatch t
  "Needed to suppress logging in testing.")

(defun elnode--auth-entry->dispatch-table (auth-scheme &optional hostpath)
  "Make a dispatch table from the AUTH-SCHEME.

If HOSTPATH is specified then the resulting match spec is of the
`hostpath' type for use with `elnode-hostpath-dispatcher'."
  (let* ((auth-scheme (gethash
                       auth-scheme
                       elnode--defined-authentication-schemes))
         (redirect (plist-get auth-scheme :redirect))
         (login-handler (plist-get auth-scheme :login-handler)))
    (when redirect
      (list
       (cons
        (concat (if hostpath "^.*/" "^") redirect "$")
        login-handler)))))

(defun* elnode--dispatch-proc (httpcon
                              path
                              url-mapping-table
                              &key
                              (function-404 'elnode-send-404)
                              (log-name "elnode")
                              extra-table)
  "Dispatch to the matched handler for the PATH on the HTTPCON.
The handler for PATH is matched in the URL-MAPPING-TABLE via
`elnode--mapper-find'.

If no handler is found then a 404 is attempted via FUNCTION-404,
if it's found to be a function, or as a last resort
`elnode-send-404'.

The function also supports the searching of the map provided by
an EXTRA-TABLE.  This is useful for authentication and other
wrappers.  If it is specified it is searched first."
  (let ((handler-func
         (or
          ;; Either a match from extra-table ...
          (and extra-table
               (elnode--mapper-find
                httpcon path extra-table))
          ;; ... or from the standard url-mapping-table
          (elnode--mapper-find
           httpcon path url-mapping-table))))
    (when elnode--do-access-logging-on-dispatch
      (elnode/con-put httpcon :elnode-access-log-name log-name))
    (cond
     ;; If we have a handler, use it.
     ((functionp handler-func)
      (funcall handler-func httpcon))
     (t
      (funcall function-404 httpcon)))))

(defun* elnode-dispatcher (httpcon
                           url-mapping-table
                           &key
                           (function-404 'elnode-send-404)
                           auth-scheme)
  "Dispatch HTTPCON to the function mapped in URL-MAPPING-TABLE.

URL-MAPPING-TABLE is an alist of:

 (url-regex . function-to-dispatch)

To map the root url you should use:

  \"^/$\"

To ensure paths end in /, `elnode-dispatcher' uses
`elnode-normalize-path'.  To map another url you should use:

  \"^/path/$\" or \"^/path/sub-path/$\"

An example server setup:

  (defun my-server (httpcon)
    (elnode-dispatcher
     httpcon
     '((\"^/$\" . root-view)
       (\"^/1/$\" . view-1))))

If FUNCTION-404 is specified it is called when no regexp is
matched, otherwise `elnode-send-404' is used.

AUTH-SCHEME is an optional authentication scheme, defined with
`elnode-defauth', which specifies a redirect mapping for
authentications."
  (elnode-normalize-path
   httpcon
   (lambda (httpcon)
     ;; Get pathinfo again because we may have redirected.
     (let ((pathinfo (elnode-http-pathinfo httpcon))
           (extra-table
            (elnode--auth-entry->dispatch-table auth-scheme)))
       (elnode--dispatch-proc
        httpcon
        pathinfo
        url-mapping-table
        :function-404 function-404
        :extra-table extra-table)))))

(defun elnode--hostpath (host path)
  "Turn the host and path into a hostpath."
  (format
   "%s/%s"
   (let ((host-name (or host "")))
     ;; Separate the hostname from any port in the host header
     (save-match-data
       (if (string-match "\\([^:]+\\)\\(:[0-9]+.*\\)*" host-name)
           (match-string 1 host-name)
           "")))
   path))

(defun* elnode-hostpath-dispatcher (httpcon
                                   hostpath-mapping-table
                                   &key
                                   (function-404 'elnode-send-404)
                                   (log-name "elnode")
                                   auth-scheme)
  "Dispatch HTTPCON to a handler based on the HOSTPATH-MAPPING-TABLE.

HOSTPATH-MAPPING-TABLE has regexs of the host and the path double
slash separated, thus:

 (\"^localhost//pastebin.*\" . pastebin-handler)

FUNCTION-404 should be a 404 handling function, by default it's
`elnode-send-404'.

LOG-NAME is an optional log-name.

AUTH-SCHEME is an optional authentication scheme, defined with
`elnode-defauth', which specifies a redirect mapping for
authentications."
  (let ((hostpath (elnode--hostpath
                   (elnode-http-header httpcon "Host")
                   (elnode-http-pathinfo httpcon)))
        (extra-table
         ;; Make sure it's a hostpath type
         (elnode--auth-entry->dispatch-table auth-scheme t)))
    (elnode--dispatch-proc
     httpcon
     hostpath
     hostpath-mapping-table
     :function-404 function-404
     :log-name log-name
     :extra-table extra-table)))

;;;###autoload
(defcustom elnode-hostpath-default-table
  '(("[^/]+//wiki/\\(.*\\)" . elnode-wikiserver)
    ("[^/]+//\\(.*\\)" . elnode-webserver))
  "Defines mappings for `elnode-hostpath-default-handler'.

This is the default mapping table for Elnode, out of the box. If
you customize this then elnode will serve these hostpath mappings
by just loading Elnode.

By default the table maps everything to
`elnode-webserver'. Unless you're happy with the default you
should probably get rid of the everything path because it will
interfere with any other mappings you add."
  :group 'elnode
  :type '(alist :key-type string
                :value-type symbol))

(defun elnode-hostpath-default-handler (httpcon)
  "A default hostpath handler.

This uses the `elnode-hostpath-default-table' for the match
table.  It calls `elnode-hostpath-dispatcher' with
`elnode-hostpath-default-table'."
  (elnode-hostpath-dispatcher httpcon elnode-hostpath-default-table))


;; Async handling stuff

(defmacro with-stdout-to-elnode (httpcon &rest body)
  "Execute BODY so that any output gets sent to HTTPCON."
  (declare
   (debug (sexp &rest form))
   (indent defun))
  (let ((hv (make-symbol "httpconvar"))
        (val (make-symbol "value")))
    `(with-temp-buffer
       (let ((,hv ,httpcon)
             (standard-output (current-buffer)))
         (let ((,val (progn ,@body)))
           (elnode-http-return ,hv (buffer-string)))))))


;; Elnode child process functions

(defcustom elnode-log-worker-elisp nil
  "If true then worker Elisp (Elisp run in a child-Emacs process) is logged.

The buffer '* elnode-worker-elisp *' is used for the log."
  :group 'elnode
  :type '(boolean))

(defcustom elnode-log-worker-responses nil
  "If true then worker Elisp logs responses in a buffer.

The buffer '* elnode-worker-response *' is used for the log."
  :group 'elnode
  :type '(boolean))


(defun elnode-wait-for-exit (process)
  "Wait for PROCESS status to go to 'exit."
  (while (not (eq (process-status process) 'exit))
    (sleep-for 1)))


;; TODO: handle errors better than messaging
(defun elnode--child-process-sentinel (process status)
  "A sentinel for Elnode child PROCESS.

Elnode child processes are just Emacs asynchronous processes that
send their output to an Elnode HTTP connection.

The main job of this sentinel is to monitor when the STATUS of
PROCESS indicates the end of the PROCESS and to do
`elnode-http-end' on the associated HTTP connection when that
happens."
  (let ((httpcon (process-get process :elnode-httpcon)))
    (cond
      ((equal status "finished\n")
       (let ((httpcon-status (process-status httpcon))
             (proc-status (process-status process)))
         (elnode-msg :info "elnode-child-process-sentinel: finished: %s -> %s on %s"
           httpcon-status proc-status httpcon)
         (unless (eq 'closed (process-status httpcon))
           (elnode-http-send-string httpcon "")
           (process-send-string httpcon "\r\n")
           (elnode--http-end httpcon)
           ;; Kill the httpcon
           (delete-process httpcon)
           (kill-buffer (process-buffer httpcon))
           ;; Cleanup the process
           (delete-process process)
           (kill-buffer (process-buffer process)))))
      ((string-match "exited abnormally with code \\([0-9]+\\)\n" status)
       (elnode-msg :info "elnode-child-process-sentinel: %s on %s"
         status httpcon)
       (unless (eq 'closed (process-status httpcon))
         ;; Spit out the error at the end of the content
         (when (elnode/con-get httpcon :elnode-child-process-command)
           (let ((error-message
                  (format "%s %s"
                          (elnode/con-get httpcon :elnode-child-process-command)
                          status)))
             (elnode-http-send-string httpcon error-message)))
         ;; Now close the content
         (elnode-http-send-string httpcon "")
         (process-send-string httpcon "\r\n"))
       (elnode--http-end httpcon)
       ;; Kill the httpcon
       (delete-process httpcon)
       (kill-buffer (process-buffer httpcon))
       ;; Kill the process
       (delete-process process)
       (kill-buffer (process-buffer process)))
      (t
       (elnode-msg :info "elnode-child-process-sentinel: %s on %s"
         status process)))))

(defun elnode--child-process-filter (process data)
  "A generic filter function for elnode child processes.

Elnode child processes are just Emacs asynchronous processes that
send their output to an Elnode HTTP connection.

This filter function does the job of taking the output from the
async process and finding the associated Elnode HTTP connection
and sending the data there."
  (let ((httpcon (process-get process :elnode-httpcon)))
    (elnode-msg
     :info "elnode-child-process-filter http state: %s data length: %s on %s"
     (process-status httpcon)
     (length data)
     httpcon)
    (unless (eq 'closed (process-status httpcon))
      (elnode-http-send-string httpcon data))))

(defun elnode-child-process (httpcon program &rest args)
  "Run the specified PROGRAM asynchronously sending output to HTTPCON.

PROGRAM is the path to the program to run, to be resolved by
`start-process' in the usual way.

ARGS is a list of arguments to pass to the program.

It is NOT POSSIBLE to run more than one process at a time
directed at the same http connection."
  (let* ((proc-args
          (append
           (list
            (format "%s-%s" (process-name httpcon) program)
            (format "child-proc %s-%s" (process-name httpcon) program)
            program) args))
         (p (let ((process-connection-type nil)
                  (default-directory (file-name-directory program)))
              (apply 'start-process proc-args))))
    ;; Store the program and args for later
    (elnode/con-put
        httpcon :elnode-child-process-command
        (format "%s %s" program (s-join " " args)))
    (set-process-coding-system p 'raw-text-unix)
    ;; Bind the http connection to the process
    (process-put p :elnode-httpcon httpcon)
    ;; Bind the process to the http connection
    ;;
    ;; WARNING: this means you can only have 1 child process at a time
    (elnode/con-put httpcon :elnode-child-process p)
    ;; Setup the filter and the sentinel to do the right thing with
    ;; incomming data and signals
    (set-process-filter p 'elnode--child-process-filter)
    (set-process-sentinel p 'elnode--child-process-sentinel)
    (elnode-msg :info "elnode-child-process init %s" httpcon)))


;; File management

(defcustom elnode-send-file-program "/bin/cat"
  "The program to use for sending files.

Altering this is not recomended but it may be a good hook for
certain types of debugging."
  :group 'elnode
  :type '(string))

(defvar elnode-replacements-pattern "<!##E \\(.*?\\) E##!>"
  "The regex used for replacing things.

The default regex is rather baroque.  This is because it needs to
be quite unique and there are a lot of different sorts of things
like this to be unique from.")

(defun elnode--buffer-template (file-buf replacements)
  "Template render a buffer and return a copy.

FILE-BUF is the source buffer to use, template sections marked up like:

 <!##E \\(.*?\\) E##!>

will be replaced with a value looked up in REPLACEMENTS.

REPLACEMENTS is either a hashtable or an association list.

For example:

 <title><!##E my-title E##!></title>
 <p>By <!##E my-name E##!>.</p>

with the REPLACEMENTS being:

  my-title => All things Elnode!
  my-name => Nic Ferrier

would result in the string:

  <title>All things Elnode!</title>
  <p>By Nic Ferrier</p>

being returned."
  (with-current-buffer file-buf
    (replace-regexp-in-string
     elnode-replacements-pattern
     (lambda (matched)
       (let ((match-var (match-string-no-properties 1 matched)))
         (cond
           ((hash-table-p replacements)
            (gethash match-var replacements ""))
           (t
            ;; Presume it's an alist
            (or
             (assoc-default match-var replacements nil t)
             "")))))
     (buffer-substring-no-properties (point-min)(point-max)))))

(defcustom elnode-webserver-visit-file (eq system-type 'windows-nt)
  "Whether the webserver reads files by visiting buffers or not.

When set to `t' files to be sent with the `elnode-send-file' are
read into Emacs using `find-file'."
  :group 'elnode
  :type 'boolean)

(defvar elnode-replacements-httpcon nil
  "This is bound by `elnode-send-file' when doing replacements.

It should not be used otherwise.")

(defvar elnode-replacements-targetfile nil
  "This is bound by `elnode-send-file' when doing replacements.

It should not be used otherwise.")

(defun elnode--rfc1123-date (time)
  "Return TIME in RFC1123 format, suitable for HTTP dates."
  (let* ((day-names '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
	 (month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
			"Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	 (decoded-time (decode-time time))
	 (day (nth (nth 6 decoded-time) day-names))
	 (month (nth (- (nth 4 decoded-time) 1) month-names)))
    (format "%s, %s %s %s"
	    day
	    (format-time-string "%d" time t)
	    month
	    (format-time-string "%Y %H:%M:%S GMT" time t))))

(defalias 'elnode-rfc1123-date 'elnode--rfc1123-date)

(defsubst elnode--file-modified-time (file)
  "Get modification time for FILE."
  (nth 5 (file-attributes file)))

(defvar elnode-send-file-assoc nil
  "A-list of file patterns vs functions to serve files.

When a file is sent with `elnode-send-file' we try and match the
targetfile against the regex patterns in the `car' of this alist
and then use the function in the `cdr' to send the file instead
of sending it directly.")

(defsubst elnode--send-file-use-child (filename replacements)
  "When to use a child process for sending files."
  (or elnode-webserver-visit-file
      replacements
      (< (elt (file-attributes filename) 7) 5000)))

(defun* elnode-send-file (httpcon targetfile
                                  &key
                                  preamble
                                  mime-types
                                  replacements)
  "Send the TARGETFILE to the HTTPCON.

If the TARGETFILE is relative then resolve it via the current
`load-file-name' or `buffer-file-name' or `default-directory'.

WARNING: this resolution order is likely to change because,
especially when developing `default-directory' can be quite
random (change buffer, change `default-directory').

Optionally you may specify extra keyword arguments:

:PREAMBLE a string of data to send before the file.

:PREAMBLE is most useful for prefixing syntax to some other file,
for example you could prefix an XML file with XSL transformation
statements so a compliant user-agent will transform the XML.

:MIME-TYPES is an optional alist of MIME type mappings to help
resolve the type of a file.

If :REPLACEMENTS is specified it should be a hash-table or an
association list used to supply values for templating.  When
templating is specified the targetfile is not sent directly but
opened in Emacs as a buffer and transformed through the
templating system before being sent.  See
`elnode--buffer-template' for details of templating.

REPLACEMENTS can optionally be a function in which case the
return value is expected to be the hash-table or alist for the
variables.  The function should have no arguments but two
variables are bound during the function's execution
`elnode-replacements-httpcon' is the `httpcon' and
`elnode-replacements-targetfile' is the targetfile to be
delivered.

See `elnode-send-file-assoc' for more possible transformations."
  (let ((filename
         (if (not (file-name-absolute-p targetfile))
             (let ((dir (or load-file-name buffer-file-name)))
               (file-relative-name
                targetfile
                (if dir (directory-file-name dir) default-directory)))
           targetfile)))
    (if (not (file-exists-p filename))
        ;; FIXME: This needs improving so we can handle the 404
        ;; This function should raise an exception?
        (elnode-send-404 httpcon)
        ;; Else ...
        (let (send-func)
          (if (setq send-func
                    (and elnode-send-file-assoc
                         (loop for (pattern . func) in elnode-send-file-assoc
                            if (string-match-p pattern targetfile)
                            return func)))
              (funcall send-func httpcon targetfile)
              ;; Else we don't have a send func so just send it
              (let ((mimetype
                     (or (when (listp mime-types)
                           (car (rassoc
                                 (file-name-extension targetfile)
                                 mime-types)))
                         (mm-default-file-encoding targetfile)
                         "application/octet-stream")))
                (elnode-http-start
                 httpcon 200
                 `("Content-type" . ,mimetype)
                 `("Last-Modified" . ,(elnode--rfc1123-date
                                       (elnode--file-modified-time targetfile))))
                (when preamble (elnode-http-send-string httpcon preamble))
                (if (elnode--send-file-use-child filename replacements)
                    (elnode-http-return
                     httpcon
                     (if replacements
                         (elnode--buffer-template
                          (find-file-noselect filename)
                          (if (functionp replacements)  ; Replacements handling
                              (let ((elnode-replacements-httpcon httpcon)
                                    (elnode-replacements-targetfile targetfile))
                                (funcall replacements))
                              replacements))
                         ;; No processing of the file, just send it
                         (with-temp-buffer 
                           (insert-file-contents-literally filename)
                           (buffer-string))))
                    ;; Otherwise use a child process
                    (elnode-child-process
                     httpcon
                     elnode-send-file-program
                     (expand-file-name targetfile)))))))))

(defmacro elnode-method (httpcon &rest method-mappings)
  "Map the HTTP method.

Write code like this:

 (elnode-method
  (GET
   (code)
   (more code))
  (POST
   (different code)
   (evenmorecode)))

See `elnode-http-params' and `elnode-http-param' for how to get
data from a POSTed body."
  (declare
   (debug (sexp &rest (sexp &rest form)))
   (indent 1))
  (let* ((var (make-symbol "v"))
         (conv (make-symbol "con")))
     `(let* ((,conv ,httpcon)
             (,var (intern (elnode-http-method ,conv))))
       (cond
        ,@(loop
           for d in method-mappings
           unless (eq (car d) t)
           collect `((eq ,var (quote ,(car d)))
                     ,@(cdr d)))
        ;; If we don't map then send an error
        ;;
        ;; probably should be 405
        (t
         ,@(or (cdr (assoc t method-mappings))
               `((elnode-send-500 ,conv))))))))


;; Make simple handlers automatically

(defun elnode-make-redirecter (location &optional type)
  "Make a handler that will redirect to LOCATION.

Optionally, use the specified TYPE as the status code, eg:

 (elnode-make-redirect \"http://somehost.com/\" 301)"
  (lambda (httpcon)
    (elnode-send-redirect httpcon location type)))

(defun* elnode-make-send-file  (filename
                                &key
                                preamble
                                mime-types
                                replacements
                                replacements-pattern)
  "Make a handler that will serve a single FILENAME.

If the FILENAME is relative then it is resolved against the
package's `load-file-name'.

Optionally MIME-TYPES and other additional keyword arguments may be
specified and are passed through, see `elnode-send-file' for
details.

The REPLACEMENTS parameter can be a function that returns a
hash-table or alist, this is very useful for this function
because it allows dynamic variables to be defined.  Again, see
`elnode-send-file' for full documentation of this feature.

The REPLACEMENTS-PATTERN can be used to set the regex used to
match replacements.  See `elnode-replacements-pattern'."
  (lambda (httpcon)
    (let ((elnode-replacements-pattern
           (or replacements-pattern
               elnode-replacements-pattern)))
      (elnode-send-file
       httpcon
       filename
       :mime-types mime-types
       :preamble preamble
       :replacements replacements))))


;; Docroot protection

(defun elnode-under-docroot-p (target-file doc-root &optional ignore-missing)
  "Is the TARGET-FILE under the DOC-ROOT?
Optional argument IGNORE-MISSING will inhibit checks for missing files."
  (let ((docroot (directory-file-name (expand-file-name doc-root))))
    (and
     (string-match
      (format "^%s\\($\\|/\\)" docroot)
      target-file)
     (or ignore-missing (file-exists-p target-file)))))

(defalias 'elnode--under-docroot-p 'elnode-under-docroot-p)

(defun elnode-not-found (httpcon target-file)
  "`elnode-docroot-for' calls this when the doc was not found.

You can override this in tests to have interesting effects.  By
default it just calls `elnode-send-404'."
  (elnode-send-404 httpcon))

(defun elnode-modified-since (httpcon modified-time)
  "Implement the HTTP If-Modified-Since test.

MODIFIED-TIME is the time the resource was modified, for example
a file modification time."
  (let* ((modified-since
          (elnode-http-header
           httpcon 'if-modified-since :time))
         ;; Now make lower precision forms, just to seconds
         (mod-since (-take 2 modified-since))
         (mod-time (-take 2 modified-time)))
    (and
     modified-since
     (or
      (time-less-p mod-time mod-since)
      (equal mod-time mod-since)))))

(defmacro elnode-etag (httpcon &rest body)
  "A macro to implement the Etag cache algorithm.

If the environment variable \"ETAG\" is present then we generate a
bound symbol `etag' in scope for BODY by `sha1'-ing the current
ETAG and the pathinfo from the HTTPCON.

Additionally, if the Etag header \"if-none-match\" is present
then we check that against the generated `etag' value and use
`elnode-cached' to send a response if they match.

If there is no \"ETAG\" environment variable or if the generated
`etag' and a presented Etag do not match then the BODY is
evaluated."
  (declare (debug (sexp &rest form))
           (indent 1))
  (let ((etag-check-v (make-symbol "etag-check")))
    `(let ((,etag-check-v (or (elnode-http-header httpcon 'if-none-match) "NONE"))
           (etag (when (getenv "ETAG")
                   (sha1 (concat
                          (getenv "ETAG")
                          (elnode-http-pathinfo httpcon))))))
       (if (and etag (equal ,etag-check ,etag))
           (elnode-cached httpcon)
           ,@body))))

(defun elnode-cached-p (httpcon target-file)
  "Is the specified TARGET-FILE older than the HTTPCON?

This uses `elnode-modified-since'."
  (elnode-modified-since
   httpcon (elnode--file-modified-time target-file)))

(defun elnode-cached (httpcon)
  "`elnode-docroot-for' calls this when the resources was cached.

By default it just calls `elnode-send-status' with 304."
  (elnode-http-start httpcon 304)
  (elnode-http-return httpcon ""))

(defvar elnode-docroot-for-no-404 nil
  "When set to true `elnode-docroot-for' doesn't check for missing files.")

(defvar elnode-docroot-for-no-cache nil
  "When set to true `elnode-docroot-for' doesn't check for cached files.")

(defmacro elnode-docroot-for (doc-root with target-file-var
                                       on httpcon
                                       do &rest handling)
  "Docroot protection for Elnode handlers.

Test the path requested in HTTPCON is safely under the DOC-ROOT
specified, bind the TARGET-FILE-VAR to the resulting expanded
file name and execute the HANDLING code.

For example:

  (elnode-docroot-for
        \"~/work\"
        with file-var
        on httpcon
        do
        (elnode-send-file httpcon file-var))

checks any resource requested in HTTPCON is a file under the
doc-root \"~/work\" and if it is, binds the resulting file name
to FILE-VAR and calls the code following DO (which sends the file
to the HTTPCON).

When a file is not found (or not safe to return) `elnode-not-found' is called.

When a file is cached on the client (when a client sends a
conditional GET for the file that shows the client has an up to
date copy) then `elnode-cached' is called."
  (declare
   (debug (sexp "with" sexp "on" sexp "do" &rest form))
   (indent defun))
  (let ((dr (make-symbol "docroot"))
        (con (make-symbol "httpcon")))
    (assert (or (eq with 'with) (eq with :with)))
    (assert (or (eq on 'on)     (eq on :on)))
    (assert (or (eq do 'do)     (eq do :do)))
    `(let ((,dr ,doc-root)
           (,con ,httpcon))
       (let ((,target-file-var (elnode-get-targetfile ,con ,dr)))
         (if (not (elnode-under-docroot-p
                   ,target-file-var ,dr elnode-docroot-for-no-404))
             (elnode-not-found ,con ,target-file-var)
           (if (and (not elnode-docroot-for-no-cache)
                    (elnode-cached-p ,con ,target-file-var))
               (elnode-cached ,con)
             ,@handling))))))


;; Webserver stuff

(defconst elnode-webserver-docroot-default
  (file-name-as-directory
   (expand-file-name "public_html" elnode-config-directory))
  "The default location of the website.

This is used to detect whether elnode needs to create this
directory or not.")

(defcustom elnode-webserver-docroot
  elnode-webserver-docroot-default
  "The document root of the webserver.

Webserver functions are free to use this or not.  The
`elnode-webserver' function does use it."
  :group 'elnode
  :type 'file)

(defcustom elnode-webserver-extra-mimetypes
  '(("text/plain" . "creole")
    ("text/plain" . "el"))
  "Extra mime types to identify special file types.

This is just a way of hacking the mime type discovery so we can
add more file mappings more easily than editing `/etc/mime.types'."
  :group 'elnode
  :type '(alist :key-type string
                :value-type string))

(defcustom elnode-webserver-index '("index.html" "index.htm")
  "A list of possible index filenames.

Anyone of the values of this list may be picked as the index page
for a directory."
  :group 'elnode
  :type '(repeat string))

(defun elnode--webserver-setup ()
  "Setup the Elnode webserver by making a default public_html dir.

The server has a single `test.html' file, this is so we can show
off the standard webserver indexing in elnode's webserver."
  (elnode--dir-setup elnode-webserver-docroot
                     elnode-webserver-docroot-default
                     "default-webserver-test.html"
                     "test.html"
                     "default-webserver-image.png"))

(defun elnode-url-encode-path (path)
  "Return a url encoded version of PATH.

This is like `url-hexify-string' but it handles the parts of the
PATH properly.  It also hexifies single quote."
  (replace-regexp-in-string
   "'" "%27"
   (mapconcat
    'identity
    (loop
       for part in (split-string path "/")
       collect
         (concat
          (url-hexify-string part)))
    "/")))

(defcustom elnode-webserver-index-page-template "<html>
 <head>
  <meta charset=\"utf-8\"></meta>
  <title>%s</title>
 </head>
 <body>
  <h1>%s</h1>
  <div>%s</div>
 </body>
</html>
"
  "The page template used to render an index page.

The order of the variables is:

- the title of the document
- the title of the document
- the HTML formatted list of files."
  :group 'elnode
  :type '(string))

(defcustom elnode-webserver-index-file-template "<a href='%s'>%s</a><br/>\r\n"
  "The template for each file in the webserver index.

This is used to display each file in an automated directory index.

It is expected the template has 2 %s variables in it, the first
is the url to link to and the second is the content of the link."
  :group 'elnode
  :type '(string))

(defun elnode--webserver-index (docroot targetfile pathinfo &optional match)
  "Constructs index documents.

The index is made for the DOCROOT and TARGETFILE. The web path is
PATHINFO.

Optional MATCH is passed directly through to
`directory-files-and-attributes'."
  ;; TODO make this usable by people generally
  (let ((dirlist (directory-files-and-attributes targetfile nil match)))
    ;; TODO make some templating here so people can change this
    (format
     elnode-webserver-index-page-template
     pathinfo
     pathinfo
     (loop for dir-entry in dirlist
           concat
           (let ((entry
                  (format
                   "%s/%s"
                   (if (equal pathinfo "/")  "" pathinfo)
                   (car dir-entry))))
             (format
              elnode-webserver-index-file-template
              (elnode-url-encode-path entry)
              (car dir-entry)))))))

;;;###autoload
(defun elnode--webserver-handler-proc (httpcon docroot mime-types)
  "Actual webserver implementation.

Do webserving to HTTPCON from the DOCROOT using the MIME-TYPES
for meta information.

This is not a real handler (because it takes more than the
HTTPCON) but it is called directly by the real webserver
handlers."
  (elnode-docroot-for docroot
    with targetfile
    on httpcon
    do
    (let ((pathinfo (elnode-http-pathinfo httpcon)))
      (if (file-directory-p targetfile)
          ;; Use an existing index file or send a directory index
          (let* ((indexfile
                  (loop for i in elnode-webserver-index
                        if (member i (directory-files targetfile))
                        return i)))
            (if indexfile
                (elnode-send-file httpcon (concat targetfile "/" indexfile))
                ;; Else send an index
                (let ((index (elnode--webserver-index
                              docroot
                              targetfile
                              pathinfo)))
                  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
                  (elnode-http-return httpcon index))))
          ;; The target is not a directory so send the file.
          (elnode-send-file
           httpcon
           targetfile
           :mime-types mime-types)))))

(defun elnode-webserver-handler-maker (&optional docroot extra-mime-types)
  "Make a webserver handler possibly with the DOCROOT and EXTRA-MIME-TYPES.

Returns a proc which is the handler. The handler serves files out
of the docroot and marks them with the content types that Emacs
knows about. You can add extra content types for the webserver
just by supplying an alist of mime-types and extensions for
EXTRA-MIME-TYPES.

The webserver handler also creates file indexes.

The webserver uses `elnode-test-path' to make sure that the
request does not go above the DOCROOT."
  (let ((my-docroot (or docroot elnode-webserver-docroot))
        (my-mime-types (or extra-mime-types
                           elnode-webserver-extra-mimetypes)))
    `(lambda (httpcon)
       ,(format "Webserver serving files on %s" my-docroot)
       (elnode--webserver-handler-proc
        httpcon ,my-docroot (quote ,my-mime-types)))))


(defvar elnode--make-webserver-store nil
  "Alist of webservers made by `elnode-make-webserver'.

Stored as `docroot' . `webserver'.")

;;;###autoload
(defun elnode-make-webserver (docroot port &optional host)
  "Make a webserver interactively, for DOCROOT on PORT.

An easy way for a user to make a webserver for a particular
directory."
  (interactive
   (let ((docroot (read-directory-name "Docroot: " nil nil t))
         (port (read-from-minibuffer "Port: "))
         (host (if current-prefix-arg
                   (read-from-minibuffer "Host: ")
                   elnode-init-host)))
     (list docroot port host)))
  (let ((webserver-proc (elnode-webserver-handler-maker docroot)))
    (add-to-list
     'elnode--make-webserver-store
     (cons docroot webserver-proc))
    (elnode-start
     webserver-proc
     :port (string-to-number (format "%s" port))
     :host host)))

;;;###autoload
(defun elnode-webserver (httpcon)
  "A simple webserver that serves documents out of `elnode-webserver-docroot'.

This is just an example of an elnode webserver, but it may be all
that is needed most of the time.

See `elnode-webserver-handler-maker' for more possibilities for
making webserver functions.

HTTPCON is the HTTP connection to the user agent."
  (elnode--webserver-setup)
  (let (use-webserver-handler-maker)
    (if use-webserver-handler-maker
        (elnode--webserver-handler-proc
         httpcon
         elnode-webserver-docroot
         elnode-webserver-extra-mimetypes)
        ;; Otherwise DO use the handler maker...
        (let ((webserver (elnode-webserver-handler-maker
                          elnode-webserver-docroot
                          elnode-webserver-extra-mimetypes)))
          (funcall webserver httpcon)))))

;; Default elnode auth databases

(defconst elnode-auth-db-spec-default
  `(db-hash
    :filename
    ,(expand-file-name (concat elnode-config-directory "elnode-auth")))
  "The default elnode-auth-db specification.")

(defcustom elnode-auth-db-spec
  elnode-auth-db-spec-default
  "The `db' specification of where the auth db is."
  :group 'elnode
  :type '(list symbol symbol string))

(defvar elnode-auth-db
  (db-make elnode-auth-db-spec)
  "Authentication database.

This is the data structure storing hashed passwords against
username keys.

It is an elnode database which can be one of several
implementations.")

(defvar elnode-secret-key "secret"
  "Secret key used to hash secrets like passwords.")

(defun elnode-auth-make-hash (username password)
  "Hash the `elnode-secret-key' and the USERNAME and PASSWORD.

This is not an ideal hashing function because `elnode-secret-key'
is not very customizable.  We need to find a way of making a
secret key per elnode app and communicating that to this kind of function.

It is possible to use a different hashing function when you
define an elnode-auth scheme and that's probably the best way to
do it right now."
  (sha1 (format "%s:%s:%s"
                elnode-secret-key
                username
                password)))

(defvar elnode--auth-user-add-databases-history nil
  "The history of symbols used for auth databases.")

(defvar elnode--auth-user-add-username-history nil
  "The history of usernames used for auth databases.")

(defun elnode-auth-user-add (username password &optional auth-db)
  "Command to add a user to the internal authentication database.

With prefix-arg also request the authentication database variable
name.  The authentication database must exist.  By default the
main `elnode-auth-db' is used."
  (interactive
   (list (read-from-minibuffer
          "username: " nil nil nil
          'elnode--auth-user-add-username-history)
         (read-passwd "password: ")
         (when current-prefix-arg
           (intern
            (completing-read
             "auth database variable (elnode-auth-user-db): "
             obarray
             nil t nil
             'elnode--auth-user-add-databases-history
             'elnode-auth-db)))))
  (unless auth-db
    (setq auth-db 'elnode-auth-db))
  (db-put
   username
   `(("token" . ,(elnode-auth-make-hash username password))
     ("username" . ,username))
   (symbol-value auth-db))
  (message "username is %s" username))

(defun* elnode-auth-user-p (username
                            password
                            &key
                            auth-test
                            (make-hash 'elnode-auth-make-hash))
  "Does the AUTH-TEST pass?

The password is stored in the db hashed keyed by the USERNAME,
this looks up and tests the hash.

MAKE-HASH is `elnode-auth-make-hash' by default.  It takes a
username and password and returns a token.  Implementing a
different function can implement different hashing algorithms.

AUTH-TEST is passed a username and must return a token.
AUTH-TEST can be used to change the hashed token lookup to find
the token in a particular database."
  (let ((token (funcall (or make-hash 'elnode-auth-make-hash)
                        username password)))
    (equal token (funcall auth-test username))))


(defvar elnode-loggedin-db (make-hash-table :test 'equal)
  "Stores logins - authentication sessions.

See `elnode-auth-login' for how this is updated.")


(progn
  ;; Sets up the elnode auth errors
  (put 'elnode-auth-credentials
       'error-conditions
       '(error elnode elnode-auth elnode-auth-credentials))
  (put 'elnode-auth-credentials
       'error-message
       "Elnode authentication failed")

  ;; For failing cookies
  (put 'elnode-auth-token
       'error-conditions
       '(error elnode elnode-auth elnode-auth-token))
  (put 'elnode-auth-token
       'error-message
       "Elnode authentication failed"))

(defun* elnode-auth-login (username
                           password
                           &key
                           auth-test
                           make-hash
                           (loggedin-db elnode-loggedin-db))
  "Log a user in.

Check the USERNAME and PASSWORD with `elnode-auth-user-p' and
then update `elnode-loggedin-db' with the username and the login
record.

When the authentication test fails `elnode-auth-credentials'
signal is raised.

The optional AUTH-TEST which is the test to check the username
and password with.  It is passed to `elnode-auth-user-p'.

The optional MAKE-HASH is a hash generation function passed to
`elnode-auth-user-p'.

LOGGEDIN-DB is the logged-in state database to use.  By default,
this is `elnode-loggedin-db'."
  (if (elnode-auth-user-p username password
                          :auth-test auth-test :make-hash make-hash)
      (let* ((rndstr (format "%d" (random)))
             (str (format "%s:%s:%s" username rndstr elnode-secret-key))
             (hash (sha1 str))
             (user-record
              (list
               :user username
               :token rndstr
               :hash hash)))
        ;; Treat loggedin-db as a hash of lists
        (let ((found (gethash username loggedin-db)))
          (if (not found)
              (puthash username (list user-record) loggedin-db)
              (setcdr found (cons user-record (cdr found)))))
        ;; Return the hash value
        hash)
      ;; Else it was bad so throw an error.
      (signal 'elnode-auth-credentials (list username password))))

(defun* elnode-auth-check-p (username
                             token
                             &key
                             (loggedin-db elnode-loggedin-db))
  "Check login status of the USERNAME against the hashed TOKEN.

Optionally use the LOGGEDIN-DB supplied.  By default this is
`elnode-loggedin-db'.

Returns USERNAME if true and `nil' if not it fails."
  ;; Treat loggedin-db as a hash of lists
  (catch :found
    (dolist (record (gethash username loggedin-db))
      (when (equal token (plist-get record :hash))
        (throw :found username)))))

(defun elnode-auth-cookie-decode (cookie-value)
  "Decode an encoded elnode auth COOKIE-VALUE.

Returns a cons of `username' and `token'"
  (when (string-match "\\(.*\\)::\\(.*\\)" cookie-value)
    (cons (match-string 1 cookie-value)
          (match-string 2 cookie-value))))

(defun* elnode-auth-get-cookie-value (httpcon &key (cookie-name "elnode-auth"))
  "Return the decoded value for COOKIE-NAME.

By default it's \"elnode-auth\" but you should use whatever
cookie-name you're using for your app."
  (let* ((cookie-value (elnode-http-cookie httpcon cookie-name t))
         (decoded-cons (elnode-auth-cookie-decode (or cookie-value ""))))
    decoded-cons))

(defun* elnode-auth-cookie-check-p (httpcon
                                    &key
                                    (cookie-name "elnode-auth")
                                    (loggedin-db elnode-loggedin-db))
  "Check that the user is loggedin according to the cookie.

The name of the cookie can be supplied with :COOKIE-NAME - by
default is is \"elnode-auth\".

LOGGEDIN-DB can be a loggedin state database which is a
hash-table.  By default it is `elnode-loggedin-db'.

Returns the username that authenticated or `nil' if it did not or
signal's an `elnode-auth-token' error with the COOKIE-NAME if
that cookie was not found."
  (let ((cookie-cons (elnode-auth-get-cookie-value
                      httpcon :cookie-name cookie-name)))
    (if (not cookie-cons)
        (signal 'elnode-auth-token cookie-name)
        ;; Else check the username and token
        (let ((username (car cookie-cons))
              (token (cdr cookie-cons)))
          (elnode-auth-check-p username token :loggedin-db loggedin-db)))))

(defun* elnode-auth-cookie-check (httpcon &key
                                          (cookie-name "elnode-auth")
                                          (loggedin-db elnode-loggedin-db))
  "Check the COOKIE-NAME has a loggedin cookie in LOGGEDIN-DB.

Signals `elnode-auth-token' on cookie or authentication failure.

See `elnode-auth-cookie-check-p' for more details."
  (or (elnode-auth-cookie-check-p
       httpcon
       :cookie-name cookie-name
       :loggedin-db loggedin-db)
      ;; Not sure this is the correct token...
      (signal 'elnode-auth-token :not-logged-in)))

(defvar elnode-auth-httpcon nil
  "Dynamic scope variable for HTTP con while we auth.")

(defun* elnode-auth-http-login (httpcon
                                username password logged-in
                                &key
                                (cookie-name "elnode-auth")
                                auth-test
                                make-hash
                                (loggedin-db elnode-loggedin-db))
  "Log the USERNAME in on the HTTPCON if PASSWORD is correct.

If authentication succeeds set the relevant cookie and redirect
the user to LOGGED-IN.

Actually uses `elnode-auth-login' to do the assertion.
`elnode-auth-credentials' is signaled by that if the assertion fails.

AUTH-DB is a database, by default `elnode-auth-db', it's passed
to `elnode-auth-login'.

AUTH-TEST and MAKE-HASH are both optional and passed down to
`elnode-auth-user-p' if they exist."
  (let* ((elnode-auth-httpcon httpcon)
         (hash
          (elnode-auth-login
           username password
           :auth-test auth-test
           :make-hash make-hash
           :loggedin-db loggedin-db)))
    (elnode-http-header-set
     httpcon
     (elnode-http-cookie-make
      cookie-name
      (format "%s::%s" username hash)
      :path "/"))
    (elnode-send-redirect httpcon (or logged-in "/"))))

(defcustom elnode-auth-login-page "<html>
<head><meta charset=\"utf-8\"></meta</head>
<body>
<form method='POST' action='<!##E target E##!>'>
<input type='hidden' name='redirect' value='<!##E redirect E##!>'/>
username: <input type='text' name='username'/><br/>
password: <input type='password' name='password'/><br/>
<input type='submit' name='login'/>
</form>
</body>
</html>"
  "A standard login page, used by `elnode-auth-login-sender'."
  :group 'elnode
  :type '(string))

(defun elnode-auth-login-sender (httpcon target redirect)
  "Send the login page for auth to HTTPCON.

The login page will send its authentication request to TARGET.

The authentication will include username, password AND REDIRECT,
which is the URL to redirect to when login is successful.

This function sends the contents of the custom variable
`elnode-auth-login-page' after templating it."
  (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
  ;; It would be nice to support preambles... not sure how.
  ;;  (when preamble (elnode-http-send-string httpcon preamble))
  (elnode-http-return
   httpcon
   (with-temp-buffer
     (insert elnode-auth-login-page)
     (elnode--buffer-template
      (current-buffer)
      `(("target" . ,target)
        ("redirect" . ,redirect))))))

(defun* elnode-auth--login-handler (httpcon
                                    sender target
                                    &key
                                    auth-test ; assert not nil?
                                    make-hash
                                    (cookie-name "elnode-auth")
                                    (loggedin-db elnode-loggedin-db))
  "An authentication handler implementation.

This is the handler that is mapped to the login path, by default
\"/login/\".

SENDER is the function which will send the login page to the
user, it takes an HTTPCON and the TARGET from the call to this
and a redirect path.  The redirect path is taken from the HTTP
parameter \"redirect\".  The SENDER function must send a
'username' and 'password' HTTP parameters to this handler.  The
SENDER function may also send a \"redirect\" parameter which will
be used to HTTP redirect the user-agent on successful
authentication.

TARGET is the path that will be used as the login handler
path (the path to call this handler).

AUTH-TEST checks the hashed details of the user's password and is
passed to `elnode-auth-user-p'.

MAKE-HASH is optional and passed down to `elnode-auth-user-p' if
present."
  (elnode-method httpcon
      (GET
       (funcall sender httpcon target
                (or (elnode-http-param httpcon "redirect") "/")))
    (POST
     (let ((username (elnode-http-param httpcon "username"))
           (password (elnode-http-param httpcon "password"))
           (logged-in (elnode-http-param httpcon "redirect")))
       (condition-case err
           (elnode-auth-http-login
            httpcon
            username password logged-in
            :auth-test auth-test
            :make-hash make-hash
            :cookie-name cookie-name)
         (elnode-auth-credentials
          (elnode-send-redirect
           httpcon
           (if (not logged-in)
               target
               (format "%s?redirect=%s" target logged-in))))
         (t (elnode-msg
             :warning "elnode-auth--login-handler: unexpected error: %S"
             err)))))))

(defun elnode-auth-default-test-v001 (username database)
  "The first test function used for Elnode auth.

This uses just the keyed value of the username as the token.  We
no longer store databases like that by default."
  (db-get username database))

(defun elnode-auth-default-test (username database)
  "The default test function used for Elnode auth.

Is uses a stored alist against USERNAME, the alist should contain
the key \"token\" with a user's token.  Whatever else the alist
contains is irrelevant."
  (let ((user (db-get username database)))
    (when user
      (kva "token" user))))

(defun* elnode-auth--make-login-handler (&key
                                         (sender 'elnode-auth-login-sender)
                                         (target "/login/")
                                         auth-test
                                         make-hash
                                         ;; only used if the auth-test is not present
                                         (auth-db elnode-auth-db) 
                                         (cookie-name "elnode-auth")
                                         (loggedin-db elnode-loggedin-db))
  "Make an `elnode-auth--login-handler', binding parameters."
  (lambda (httpcon)
    (elnode-auth--login-handler
     httpcon
     sender target
     ;; Make a test function if we don't have one
     :auth-test (if (functionp auth-test)
                    auth-test
                    (lambda (username)
                      (elnode-auth-default-test username auth-db)))
     :make-hash (when (functionp make-hash) make-hash)
     :cookie-name cookie-name
     :loggedin-db loggedin-db)))

(defun* elnode-defauth (scheme-name
                        &key
                        (test :cookie)
                        auth-test
                        make-hash
                        (auth-db 'elnode-auth-db)
                        (cookie-name "elnode-auth")
                        (failure-type :redirect)
                        (redirect "/login/")
                        (sender 'elnode-auth-login-sender))
  "Define an Elnode authentication scheme.

An authentication scheme consists of the following attributes:

TEST what sort of test is used to test the authentication, by
default this is `:cookie'.  No other authentication tests are
possible right now but in the future there might be many (there
might also be a general `:function' test that allows calling of a
function to implement the test).

COOKIE-NAME is used when the TEST is `:cookie'.  It is the name
of the cookie to use for authentication.  By default this is
`elnode-auth'.  It must be specified as a string.

AUTH-TEST is a function to implement checking authentiation of
users.  It is passed a username and must respond with a token
that can be checked against the value returned by the hashing
function (see MAKE-HASH and `elnode-auth-make-hash' which is the
default hashing function).  AUTH-TEST can be nil in which case a
default will be used, based on AUTH-DB using
`elnode-auth-default-test'.

MAKE-HASH is a function to implement the construction of a hash
token for authentication.  It takes a username and password and
must produce the same value as AUTH-TEST.

AUTH-DB is the `db' used for authentication information.
It is used as the authority of information on users.  By default
this is `elnode-auth-db'.

FAILURE-TYPE is what to do if authentication fails.  Currently
only `:redirect' is supported.  To redirect on failure means to
send a 302 with a location to visit a login page.  :FAILURE-TYPE
is `:redirect' by default.

REDIRECT is where to redirect to if FAILURE-TYPE is `:redirect'.
By default this is \"/login/\".  If SENDER is not nil then a
dispatcher made with `elnode-dispatcher' or
`elnode-hostpath-dispatcher' that is told about this auth scheme
will dispatch a path naming REDIRECT to SENDER.

SENDER is an Elnode handler taking additional parameters of
`target' and `redirect'.  By default this is the function
`elnode-auth-login-sender'.  Specify a different function if you
want to totally change the login page."
  (let* ((login-handler (elnode-auth--make-login-handler
                         :sender sender
                         :target redirect
                         :auth-test auth-test
                         :make-hash make-hash
                         :auth-db auth-db
                         :cookie-name cookie-name))
         (auth-scheme (list
                       :test test
                       :cookie-name cookie-name
                       :failure-type failure-type
                       :redirect redirect
                       :login-handler login-handler)))
    (puthash scheme-name
             auth-scheme
             elnode--defined-authentication-schemes)))

(defmacro elnode-auth-dispatcher (httpcon auth-scheme &rest body)
  "Dispatch HTTPCON to AUTH-SCHEME's handler if it matches.

Otherwise do BODY."
  (declare
   (debug (sexp sexp &rest form))
   (indent 2))
  (let ((httpcon-v (make-symbol "httpcon-v"))
        (auth-scheme-v (make-symbol "auth-scheme-v"))
        (redirect-v (make-symbol "redirect-v"))
        (handler-v (make-symbol "handler-v")))
    `(let* ((,httpcon-v ,httpcon)
            (,auth-scheme-v
             (gethash
              ,auth-scheme
              elnode--defined-authentication-schemes))
            (,redirect-v (plist-get ,auth-scheme-v :redirect))
            (,handler-v (plist-get ,auth-scheme-v :login-handler)))
       (if (elnode--mapper-find-match-func
            (elnode-http-pathinfo ,httpcon-v)
            (cons ,redirect-v ,handler-v))
           ;; If the current path matches call the auth handler
           (funcall ,handler-v ,httpcon-v)
           ;; Else do whatever the body was
           ,@body))))

(defun elnode-auth-username (httpcon)
  "Return the username currently associated to the HTTPCON."
  (elnode/con-get httpcon :auth-username))

(defmacro if-elnode-auth (httpcon scheme authd &rest anonymous)
  "Check the HTTPCON for SCHEME auth and eval AUTHD.

If the auth fails then evaluate ANONYMOUS instead.

When evaling AUTHD the `:auth-username' property of the process is set
to the user who authenticated."
  (declare
   (debug (sexp sexp form body))
   (indent 2))
  (let ((httpconv (make-symbol "httpconv")))
    `(let ((,httpconv ,httpcon)
           (scheme-list
            (gethash ,scheme
                     elnode--defined-authentication-schemes)))
       (if (eq :cookie (plist-get scheme-list :test))
           (condition-case token
               (let* ((cookie (plist-get scheme-list :cookie-name))
                      (username
                       (elnode-auth-cookie-check ,httpconv :cookie-name cookie)))
                 (elnode/con-put ,httpconv :auth-username username)
                 ;; Do whatever the code was now.
                 ,authd)
             ;; On auth failure do the ELSE
             (elnode-auth-token (progn ,@anonymous)))
           ;; Not a cookie test - not sure what to do...
           (error 'elnode-not-a-cookie)))))

(defmacro with-elnode-auth (httpcon scheme &rest body)
  "Protect code with authentication using HTTPCON and SCHEME.

This macro protects code in a handler with a check for an
authenticated request (the check is configurable).  If the check
fails then an appropriate action is taken; for example, sending a
login page.

SCHEME is the authentication scheme to use as defined by
`elnode-auth-define-scheme'."
  (declare (debug (sexp sexp body))
           (indent 2))
  (let ((httpconv (make-symbol "httpconv")))
    `(let ((,httpconv ,httpcon))
       (if-elnode-auth ,httpconv ,scheme
         (progn ,@body)
         (let ((to
                (cond
                  (;; We have a wrapper... other lists other
                   ;; than wrappers are probably possible; we
                   ;; should qualify the test here to be
                   ;; wrapper specific
                   (listp (plist-get scheme-list :redirect))
                   (format
                    "%s?redirect=%s"
                    (elt (plist-get scheme-list :redirect) 3)
                    (elnode-http-pathinfo ,httpconv)))
                  ;; A plain string can be used directly
                  ((stringp (plist-get scheme-list :redirect))
                   (plist-get scheme-list :redirect))
                  (t
                   (error
                    ":redirect MUST be  a list or a string")))))
           (elnode-send-redirect ,httpconv to))))))

(defun elnode-test-login (auth target username password)
  "Send a test login to Elnode."
  ;; FIXME - use AUTH as a reference to an elnode-authentication
  ;; declaration and pull things like /login/ from it
  (elnode-test-call
   (format "/login/?redirect=%s" target)
   :method "POST"
   :parameters (list (cons "username" username)
                     (cons "password" password))))

;;;###autoload
(defun elnode-init ()
  "Bootstraps the elnode environment when the Lisp is loaded.

It's useful to have elnode start automatically... on Lisp
load.  If the variable `elnode-init-port' is set then this
function will launch a server on it.

The server is started with `elnode-hostpath-default-handler' as
the handler and listening on `elnode-init-host'"
  (interactive)
  (when elnode-init-port
    (condition-case nil
        (progn
          (elnode-start
           'elnode-hostpath-default-handler
           :port elnode-init-port
           :host elnode-init-host)
          (setq elnode--inited t))
      (error "Elnode could not initialize."))))

(defcustom elnode-do-init nil
  "Should elnode start a server on load?

The server that is started is controlled by more elnode
customizations.

`elnode-hostpath-default-table' defines the mappings from
hostpath regexs to handler functions. By default elnode ships
with this customization setup to serve the document root defined
in `elnode-webserver-docroot', which by default is ~/public_html."
  :group 'elnode
  :type '(boolean))

(defvar elnode--inited nil
  "Records when elnode is initialized.

This is autoloading mechanics, see the eval-after-load for doing
init.")


;; Make sure we add elnode/case to the keywords
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<elnode/case\\>" . 'font-lock-keyword-face)))

;;;###autoload
(add-hook 'after-init-hook
          (lambda ()
            (when (and (boundp 'elnode-do-init)
                       (symbol-value 'elnode-do-init))
              (condition-case err
                  (progn
                    (elnode-init)
                    (when (and elnode-defer-on
                               (not elnode--defer-timer))
                      (elnode--init-deferring)))
                (error "Elnode auto-start failed.")))))

(provide 'elnode)

;;; elnode.el ends here
