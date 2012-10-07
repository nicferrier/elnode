;;; elnode-wiki.el --- a wiki with Elnode  -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; This is a Wiki Engine completely written in EmacsLisp, using Elnode
;; as a server.
;;
;;; Source code
;;
;; elnode's code can be found here:
;;   http://github.com/nicferrier/elnode

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elnode-wiki--private-function
;;
;; for private functions.


;;; Code:

(require 'elnode)
(require 'db)
(eval-when-compile 'fakir)
(require 'creole nil 't)
;;(require 'vc)

(defgroup elnode-wikiserver nil
  "A Wiki server written with Elnode."
  :group 'elnode)

;;;###autoload
(defconst elnode-wikiserver-wikiroot-default
  (expand-file-name (concat elnode-config-directory "wiki/"))
  "The default location of the wiki root.

This is used to detect whether elnode needs to create this
directory or not.")

;;;###autoload
(defcustom elnode-wikiserver-wikiroot
  elnode-wikiserver-wikiroot-default
  "The root for the Elnode wiki files.

This is where elnode-wikiserver serves wiki files from."
  :type '(directory)
  :group 'elnode-wikiserver)

(defcustom elnode-wikiserver-body-header
  "<div id='top'></div>"
  "HTML BODY preamable of a rendered Wiki page."
  :type '(string)
  :group 'elnode-wikiserver)

(defcustom elnode-wikiserver-body-footer
  "<div id='footer'>
<form action='{{page}}' method='POST'>
<fieldset>
<legend>Edit this page</legend>
<textarea  cols='80' rows='20' name='wikitext'>
{{text}}
</textarea><br/>
<input type='text' name='comment' value=''/>
<input type='submit' name='save' value='save'/>
<input type='submit' name='preview' value='preview'/>
</fieldset>
</form>
</div>"
  "HTML BODY footter for a rendered Wiki page."
  :type '(string)
  :group 'elnode-wikiserver)

(defcustom elnode-wikiserver-body-footer-not-loggedin
  "<div id='footer'>
    <a href='/wiki/login/?redirect={{page}}'>login to edit</a>
  </div>"
  "HTML BODY footter for a rendered Wiki page."
  :type '(string)
  :group 'elnode-wikiserver)

(defun elnode-wiki--setup ()
  "Setup the wiki."
  (elnode--dir-setup elnode-wikiserver-wikiroot
                     elnode-wikiserver-wikiroot-default
                     "default-wiki-index.creole"
                     "index.creole"))

(ert-deftest elnode-wiki--setup ()
  "Test the wiki setup function."
  ;; Test that it's not called if we can't find the source file
  (let (called)
    (flet ((make-directory (dirname &optional parents)
             (setq called t))
           ;; We fake buffer-file-name so that the wiki-index-source
           ;; will not be found
           (buffer-file-name ()
             "/tmp/elnode/elnode-wiki.el"))
      (elnode-wiki--setup)
      (should-not called)))
  ;; Test that when called we're going to copy things right
  (let (make-dir
        copy-file
        ;; Ensure the configurable wikiroot is set to the default
        (elnode-wikiserver-wikiroot elnode-wikiserver-wikiroot-default))
    (flet ((make-directory (dirname &optional parents)
             (setq make-dir (list dirname parents)))
           (dired-copy-file (from to ok-flag)
             (setq copy-file (list from to ok-flag)))
           ;; Mock the source filename environment
           (buffer-file-name ()
             "/tmp/elnode--wiki-setup-test/elnode-wiki.el")
           (file-exists-p (filename)
             (equal
              filename
              "/tmp/elnode--wiki-setup-test/default-wiki-index.creole")))
      (elnode-wiki--setup)
      (should
       (equal
        (list
         ;; This is the dir we should make
         '("/home/nferrier/.emacs.d/elnode/wiki/" t)
         ;; This is the copy file spec
         '("/tmp/elnode--wiki-setup-test/default-wiki-index.creole"
           "/home/nferrier/.emacs.d/elnode/wiki/index.creole"
           nil))
        ;; So this is the directory that make-directory will create
        ;; and the copy-file spec
        (list make-dir copy-file))))))

(defun elnode--wiki-call (out-buf page-text page)
  "Call a wiki page sending output OUT-BUF.

The page is faked with PAGE-TEXT."
  (flet
      ((elnode--worker-lisp-helper (child-lisp)
         `((progn
             (require 'creole)
             (require 'cl)
             (flet ((creole--get-file (filename)
                      (let ((buf (get-buffer-create "wikibuf")))
                        (with-current-buffer buf
                          (insert ,page-text))
                        buf)))
               ,@child-lisp)))))
    (elnode-wait-for-exit
     (elnode-worker-elisp
         out-buf
         ((target page)
          (page-info page)
          (header elnode-wikiserver-body-header)
          (footer elnode-wikiserver-body-footer))
       (require 'creole)
       (creole-wiki
        target
        :destination t
        :variables `((page . ,page-info))
        :body-header header
        :body-footer footer)))))

;; Deprecated
(defun elnode-wiki-send (httpcon wikipage &optional pageinfo)
  "Sends the WIKIPAGE to the HTTPCON.

If PAGEINFO is specified it's the HTTP path to the Wiki page.

Uses Elnode's worker elisp stuff which is now deprecated."
  (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
  (let ((page (or pageinfo (elnode-http-pathinfo httpcon))))
    (elnode-worker-elisp
        httpcon
        ((target wikipage)
         (page-info page)
         (header elnode-wikiserver-body-header)
         (footer elnode-wikiserver-body-footer))
      (require 'creole)
      (creole-wiki
       target
       :destination t
       :variables `((page . ,page-info))
       :body-header header
       :body-footer footer))))

(define-obsolete-function-alias
    'elnode-wiki-send
    'elnode-wiki-page
  "24.1"
  "The worker elisp code that this depends on is deprecated in
  favour of Enode RLE.")

(defvar elnode-wiki-page-use-rle nil
  "Whether to use RLE for this wiki or not.

The RLE stuff is not really stable yet so this is a switch that
let's developers play with but does not affect use.")

(defun elnode-wiki-page-rle (httpcon wikipage &optional pageinfo)
  "Creole render the  WIKIPAGE to the HTTPCON.

If PAGEINFO is specified it's the HTTP path to the Wiki page.

This version uses RLE which renders the Wiki page in a child
Emacs."
  (let ((authenticated (elnode-http-cookie httpcon "elnodeauth")))
    (let ((page-info (or pageinfo (elnode-http-pathinfo httpcon)))
          (header elnode-wikiserver-body-header)
          (footer (if authenticated
                      elnode-wikiserver-body-footer
                      elnode-wikiserver-body-footer-not-loggedin)))
      (elnode-async-do
       httpcon
       requires (creole elnode)
       with-environment ((target wikipage)
                         (page-info page-info)
                         (header header)
                         (footer footer))
       do
       (creole-wiki
        target
        :destination t
        :variables `((page . ,page-info))
        :body-header header
        :body-footer footer)))))

(defun elnode-wiki-page (httpcon wikipage &optional pageinfo)
  "Creole render a WIKIPAGE back to the HTTPCON."
  (if elnode-wiki-page-use-rle
      (elnode-wiki-page-rle httpcon wikipage pageinfo)
      ;; Otherwise just do it
      (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
      (with-stdout-to-elnode httpcon
          (let ((page-info (or pageinfo (elnode-http-pathinfo httpcon)))
                (header elnode-wikiserver-body-header)
                (footer (if-elnode-auth httpcon 'elnode-wiki-auth
                          elnode-wikiserver-body-footer
                          elnode-wikiserver-body-footer-not-loggedin)))
            (creole-wiki
             wikipage
             :destination t
             :variables (list (cons 'page page-info))
             :body-header header
             :body-footer footer)))))

(defun elnode-wiki--text-param (httpcon)
  "Get the text param from HTTPCON and convert it."
  (replace-regexp-in-string
   "\r" "" ; browsers send text in DOS line ending format
   (elnode-http-param httpcon "wikitext")))

(defun elnode-wiki--save-request (httpcon wikiroot path text)
  "Process an update request."
  (let* ((page (if path
                   (save-match-data
                     (string-match "/wiki/\\(.*\\)$" path)
                     (match-string 1 path))))
         (comment (elnode-http-param httpcon "comment"))
         (file-name (if (equal page "")
                        (concat wikiroot "index.creole")
                      (concat (file-name-as-directory wikiroot) page)))
         (buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (save-buffer)
      (let ((git-buf
             (get-buffer-create
              (generate-new-buffer-name
               "* elnode wiki commit buf *"))))
        (shell-command
         (format "git commit -m '%s' %s" comment file-name)
         git-buf)
        (kill-buffer git-buf))
      (elnode-wiki-page httpcon file-name))))

(defun elnode-wiki-handler (httpcon wikiroot)
  "A low level handler for Wiki operations.

Send the Wiki page requested, which must be a file existing under
the WIKIROOT, back to the HTTPCON.

Update operations are protected by authentication."
  (elnode-method httpcon
    (GET
     (elnode-docroot-for wikiroot
       with target-path
       on httpcon
       do
       (if (equal target-path (expand-file-name (concat wikiroot "/")))
           (elnode-wiki-page httpcon (concat wikiroot "/index.creole"))
           (elnode-wiki-page httpcon target-path))))
    (POST
     (with-elnode-auth httpcon 'elnode-wiki-auth
       (let* ((path (elnode-http-pathinfo httpcon))
              (text (elnode-wiki--text-param httpcon)))
         (if (not (elnode-http-param httpcon "preview"))
             ;; A save request in which case save the new text and then
             ;; send the wiki text.
             (elnode-wiki--save-request httpcon wikiroot path text)
             ;; Might be a preview request in which case send back the WIKI
             ;; text that's been sent.
             (with-temp-file "/tmp/preview"
               (insert text))
             (elnode-wiki-send httpcon "/tmp/preview" path)))))))

;;;###autoload
(defun elnode-wikiserver-test ()
  "Test whether we should serve Wiki or not."
  (featurep 'creole))

;;;###autoload
(define-elnode-handler elnode-wikiserver (httpcon)
  "Serve Wiki pages from `elnode-wikiserver-wikiroot'.

HTTPCON is the request.

The Wiki server is only available if the `creole' package is
provided. Otherwise it will just error."
  (if (not (elnode-wikiserver-test))
      (elnode-send-500 httpcon "The Emacs feature 'creole is required.")
      (elnode-wiki--setup)
      (elnode-wiki-handler httpcon elnode-wikiserver-wikiroot)))

(defvar elnode-wiki-db
  (db-make
   `(db-hash
     :filename
     ,(expand-file-name
       (concat elnode-config-directory "elnode-wiki-auth")))))

;; Define the authentication scheme for the wiki
(elnode-auth-define-scheme
 'elnode-wiki-auth
 :auth-db elnode-wiki-db
 :redirect (elnode-auth-make-login-wrapper
            'elnode-wikiserver
            :target "/wiki/login/"))


;;; Tests

(ert-deftest elnode-wiki-page ()
  "Full stack Wiki test."
  (with-elnode-mock-server
      ;; The dispatcher function
      (lambda (httpcon)
        (let ((elnode-wikiserver-wikiroot "/home/elnode/wiki"))
          (elnode-hostpath-dispatcher
           httpcon
           '(("[^/]*//wiki/\\(.*\\)" . elnode-wikiserver))))) t
    (fakir-mock-file (fakir-file
                      :filename "test.creole"
                      :directory "/home/elnode/wiki"
                      :content "= Hello World =\nthis is a creole wiki file!\n")
        (let* ((elnode--do-error-logging nil)
               (elnode--do-access-logging-on-dispatch nil))
          (should-elnode-response
           (elnode-test-call "/wiki/test.creole")
           :status-code 200
           :body-match ".*<h1>Hello World</h1>.*")))))

(provide 'elnode-wiki)

;;; elnode-wiki.el ends here
