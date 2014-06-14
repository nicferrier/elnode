;;; elnode-js.el --- elnode js integration tools -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, hypermedia

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

;; Often we make websites with Javascript. Elnode has built in tools
;; to help.

;; elnode-js/browserify -- let's elnode take advantage of browserify
;; to simplify your javascript (by using node's require which
;; browserify translates into the correct browser code).

;;; Code:

(require 'elnode)
(require 'noflet)

(defmacro* env-let ((var value) &rest body)
  "Very quick and simple Unix ENV let."
  ;; have a copy of this in world-time-list
  (declare
   (debug (sexp &rest form))
   (indent 1))
  (let ((varv (make-symbol "varv"))
        (valuev (make-symbol "valuev"))
        (saved-var (make-symbol "saved-var")))
    `(let* ((,varv ,var)
            (,saved-var (getenv ,varv))
            (,valuev ,value))
       (unwind-protect
            (progn
              (setenv ,varv ,valuev)
              ,@body)
         (setenv ,varv ,saved-var)))))

(defun env-add-path (var value)
  "Add VALUE to env var VAR if it's not there already."
  (let ((e-val (substitute-in-file-name value)))
    (if (member e-val (split-string (getenv var) ":"))
        (getenv var)
        (setenv var (concat (getenv var) ":" e-val)))))


(defun elnode-js/node-bin ()
  "Where is the NodeJS binary?

We look in a place provided by `nodejs-repl' package or in
\"~/nodejs\", \"/usr/local/bin\" or in \"/usr/bin\" in that
order."
  (noflet ((file-exists (filename)
             (and (file-exists-p (expand-file-name filename)) filename)))
    (or (and (featurep 'nodejs-repl)
             (symbol-value 'nodejs-repl-command))
        (or (file-exists "~/nodejs/bin/nodejs")
            (file-exists "/usr/local/bin/nodejs")
            (file-exists "/usr/bin/nodejs")))))

(defun elnode-js/browserify-bin (&optional directory)
  "Where is browserify?

We search DIRECTORY, if it's supplied, and then the project root,
if there is one and then the `default-directory'."
  (let ((browserify "node_modules/.bin/browserify"))
    (noflet ((file-exists (filename)
               (and (file-exists-p (expand-file-name filename)) filename)))
      (or
       (and directory (file-exists (expand-file-name browserify directory)))
       (and (featurep 'find-file-in-project)
            (file-exists (expand-file-name
                          browserify
                          (funcall 'ffip-project-root))))
       (file-exists "node_modules/.bin/browserify")))))

(defun elnode-js/browserify (httpcon docroot path)
  "Run browserify from DOCROOT for the PATH.

Browserify is a nodejs tool that turns nodejs based Javascript
into Javascript that works inside the browser.

nodejs code can use nodejs's `require' form to import modules,
which is simpler than many client side solutions.  So browserify
solves the module problem across node.js and the browser."
  (let ((browserify (elnode-js/browserify-bin docroot))
        (nodejs (elnode-js/node-bin)))
    (when (and nodejs browserify)
      (env-let ("PATH" nodejs)
        (let ((default-directory docroot))
          (elnode-http-start httpcon 200 '(Content-type . "application/js")))
          (elnode-child-process httpcon browserify (concat docroot path))))))

(defun elnode-js-handler-demo (httpcon)
  "A demonstration server for browserify support."
  (elnode-hostpath-dispatcher
   httpcon
   '(("^[^/]*//scripts/example.js"
      . (lambda (httpcon)
          (elnode-js/browserify
           httpcon 
           (expand-file-name "~/work/bwinparty/cards")
           (elnode-http-pathinfo httpcon)))))))

;; (elnode-start 'elnode-js-handler-demo :port 8102)
;; (elnode-stop 8102)

(provide 'elnode-js)

;;; elnode-js.el ends here
