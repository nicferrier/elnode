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

;;; Code:


(defun elnode-js/npm-present? (directory)
  "Does the DIRECTORY have an NPM install?"
  (file-exists-p (concat (file-name-as-directory directory) "node_modules")))

(defun elnode-js/browserify? (directory)
  "Does the DIRECTORY have browserify?"
  (file-exists-p (concat
                  (file-name-as-directory directory)
                  "node_modules/.bin/browserify")))

(defmacro* let-env ((var value) &rest body)
  "Very quick and simple Unix ENV let."
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

(defun elnode-js/browserify (httpcon docroot path)
  "Run browserify from DOCROOT for the PATH.

Browserify is a node tool that turns node based js into stuff
that works inside the browser."
  (let-env ("PATH" (concat
                    (expand-file-name "~/nodejs/bin") ":"
                    (getenv "PATH")))
    (let ((default-directory docroot)
          (program (concat
                    (file-name-as-directory docroot)
                    "node_modules/.bin/browserify")))
      (elnode-http-start httpcon 200 '(Content-type . "application/js"))
      (elnode-child-process httpcon program (concat docroot path)))))

(defun elnode-js-handler-demo (httpcon)
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
