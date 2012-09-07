;;; elnode-db.el --- a database interface  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 30 June 2012
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

;;; Commentary:
;;
;; This is a simple database interface and implementation.
;;
;; It should be possible to specify any kind of key/value database
;; with this interface.
;;
;; The supplied implementation is an Emacs hash-table implementation
;; backed with serializing objects.  It is NOT intended for anything
;; other than very simple use cases and will not scale very well at
;; all.

;; However, other implementations (mongodb, redis or PostgreSQL
;; hstore) would be easy to implement and fit in here.

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


;;; Code:

(eval-when-compile
  (require 'cl))

(defvar elnode-db--types (make-hash-table :test 'eq)
  "Hash of database type ids against funcs?")

(defun* elnode-db-make (reference)
  "Make an elnode db based on the REFERENCE."
  (if (and (listp reference)
           (eq
            'elnode-db-hash
            (car reference)))
      ;; this should be part of what we find when we look it up?
      (elnode-db-hash reference)
      ;; Otherwise look it up...
      (let ((db-func (gethash (car reference) elnode-db--types)))
        (if (functionp db-func)
            (funcall db-func reference)
            ;; there should be a specific db error
            (error "no such database implementation")))))

(defun elnode-db-get (key db)
  "Get the value from the DB with the KEY."
  (funcall (plist-get db :get) key db))

(defun elnode-db-put (key value db)
  "Put a new VALUE into the DB with the specified KEY.

Return the VALUE as it has been put into the DB."
  (funcall (plist-get db :put) key value db))

(defun elnode-db-map (func db &optional query)
  "Call FUNC for every record in DB optionally QUERY filter.

QUERY, if specified, should be a list of query terms.

FUNC should take whatever record the DB produces.  This
specification makes no recommendations about that."
  ;; The query should be implemented here, around the func
  (funcall (plist-get db :map) func db query))


(defun elnode-db-hash (reference)
  "Make a db-hash database.

REFERENCE comes from the call to `elnode-db-make' and should
include a `:filename' key arg to point to a file:

  '(elnode-db-hash :filename \"/var/local/db/auth-db\")

If the filename exists then it is loaded into the database."
  (let* ((filename (plist-get (cdr reference) :filename))
         (db (list
              :db (make-hash-table :test 'equal)
              :get 'elnode-db-hash-get
              :put 'elnode-db-hash-put
              :map 'elnode-db-hash-map
              :filename filename)))
    (when (and filename
               (file-exists-p (concat filename ".elc")))
      (elnode-db-hash--read db))
    ;; Return the database
    db))

(defun elnode-db-hash--read (db)
  "Loads the DB."
  (let ((filename (plist-get db :filename)))
    (when filename
      (load-file (concat filename ".elc"))
      (plist-put db :db (symbol-value (intern filename))))))

(defun elnode-db-hash--save (db)
  "Saves the DB."
  (let ((filename (plist-get db :filename)))
    (when filename
      ;; Make the parent directory for the db if it doesn't exist
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))
      ;; Now store the data
      (with-temp-file (concat filename ".el")
        (erase-buffer)
        (let ((fmt-obj (format
                        "(setq %s (eval-when-compile %S))"
                        (intern filename)
                        (plist-get db :db))))
          (insert fmt-obj)))
      ;; And compile it and delete the original
      (byte-compile-file (concat filename ".el"))
      (delete-file (concat filename ".el")))))

(defun elnode-db-hash-get (key db)
  (let ((v (gethash key (plist-get db :db))))
    v))

(defun elnode-db-hash-map (func db &optional query)
  "Run FUNC for every value in DB.

The QUERY is ignored.  We never filter."
  (maphash func (plist-get db :db)))

(defun elnode-db-hash-put (key value db)
  (let ((v (puthash key value (plist-get db :db))))
    ;; Instead of saving every time we could simply signal an update
    ;; and have a timer do the actual save.
    (elnode-db-hash--save db)
    v))


;; Filter db - let's you filter another db

(defun elnode-db-filter-get (key db)
  (let* ((filter-func (plist-get db :filter))
         (origin (plist-get db :source))
         (value (elnode-db-get key origin)))
    (funcall filter-func key value)))

(defun elnode-db-filter-put (key value db)
  (let* ((filter-func (plist-get db :filter))
         (origin (plist-get db :source))
         (ret (elnode-db-put key value origin)))
    (funcall filter-func key ret)))

(defun elnode-db-filter-map (key db &optional query)
  (let* ((filter-func (plist-get db :filter))
         (origin (plist-get db :source)))
    (mapcar
     filter-func
     (elnode-db-map key origin query))))

(defun elnode-db-filter (reference)
  "Make a database object that is a filter around another.

The reference should look something like:

 '(elnode-db-filter
    :source (elnode-db-hash :filename ....)
    :filter (lambda (value) ...)

The `:filter' function takes 2 arguments: KEY and VALUE with
VALUE being the returned value from the `:source' database."
  (let* ((ref-plist (cdr reference))
         (db (list
              :get 'elnode-db-filter-get
              :put 'elnode-db-filter-put
              :map 'elnode-db-filter-map
              :filter (plist-get ref-plist :filter)
              :source (plist-get ref-plist :source))))
    db))

(puthash 'elnode-db-filter 'elnode-db-filter elnode-db--types)

(provide 'elnode-db)

;;; elnode-db.el ends here
