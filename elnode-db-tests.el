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
;; These are the tests for elnode-db.
;;
;; elnode-db needs it's own tests because elnode-db is a depend of
;; elnode itself and thus it gets loaded before elnode.  If it did not
;; have it's own tests it would cause a dependancy on ERT for all of
;; elnode.

;; This code originated from this gmane thread:
;; http://permalink.gmane.org/gmane.emacs.devel/115821 by Thierry Volpiatto

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

(eval-when-compile (require 'cl))
(require 'ert)
(require 'elnode-db)

(ert-deftest elnode-db-get ()
  "Test the database interface and the hash implementation."
  ;; Make a hash-db with no filename
  (let ((db (elnode-db-make '(elnode-db-hash))))
    (should-not (elnode-db-get "test-key" db))
    (elnode-db-put "test-key" 321 db)
    (should
     (equal
      321
      (elnode-db-get "test-key" db)))))

(ert-deftest elnode-db-hash--save ()
  "Test the saving of a hash db."
  (unwind-protect
       (progn
         (let ((db (elnode-db-make
                    ;; You shouldn't use an extension but let elnode deal
                    ;; with it.
                    '(elnode-db-hash :filename "/tmp/elnode-test-db"))))
           ;; Override the save so it does nothing from put
           (flet ((elnode-db-hash--save (db)
                    t))
             (elnode-db-put 'test1 "value1" db)
             (elnode-db-put 'test2 "value2" db))
           ;; And now save
           (elnode-db-hash--save db))
         ;; And now load in a different scope
         (let ((db (elnode-db-make
                    '(elnode-db-hash :filename "/tmp/elnode-test-db"))))
           (should
            (equal "value1"
                   (elnode-db-get 'test1 db)))))
    (delete-file "/tmp/elnode-test-db.elc")))

(provide 'elnode-db-tests)

;;; elnode-db-tests.el ends here
