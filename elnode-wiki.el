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
(require 'creole nil 't)
;;(require 'vc)

(defgroup elnode-wikiserver nil
  "A Wiki server written with Elnode."
  :group 'elnode)

(defcustom elnode-wikiserver-wikiroot "~/wiki"
  "The default root for the Elnode wiki files."
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


(defun elnode-wiki-send (httpcon wikipage &optional pageinfo)
  "A very low level Wiki handler.

Sends the WIKIPAGE to the HTTPCON.

If PAGEINFO is specified it's the HTTP path to the Wiki page."
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

;; New version of elnode-wiki-send, basically
(defun elnode-wiki-page (httpcon wikipage &optional pageinfo)
  "Creole render a WIKIPAGE back to the HTTPCON."
  (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
  (with-stdout-to-elnode httpcon
    (let ((page-info (or pageinfo (elnode-http-pathinfo httpcon)))
          (header elnode-wikiserver-body-header)
          (footer elnode-wikiserver-body-footer))
      (creole-wiki
       wikipage
       :destination t
       :variables `((page . ,page-info))
       :body-header header
       :body-footer footer))))

(defun elnode-wiki-handler (httpcon wikiroot)
  "A low level handler for Wiki operations.

Send the Wiki page requested, which must be a file existing under
the WIKIROOT, back to the HTTPCON."
  (elnode-method
    (GET
     (elnode-docroot-for wikiroot
       with target-path
       on httpcon
       do
       (if (equal target-path (concat wikiroot "/"))
           (elnode-wiki-page httpcon (concat wikiroot "/index.creole"))
         (elnode-wiki-page httpcon target-path))))
    (POST
     (let* ((path (elnode-http-pathinfo httpcon))
            (comment (elnode-http-param httpcon "comment"))
            (text (replace-regexp-in-string
                   "\r" "" ; browsers send text in DOS line ending format
                   (elnode-http-param httpcon "wikitext")))
            (page (if path
                      (save-match-data
                        (string-match "/wiki/\\(.*\\)$" path)
                        (match-string 1 path)))))
       (if (not (elnode-http-param httpcon "preview"))
           ;; A save request in which case save the new text and then
           ;; send the wiki text.
           (let* ((file-name (concat wikiroot "/" page))
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
               (elnode-wiki-send httpcon file-name)))
         ;; Might be a preview request in which case send back the WIKI
         ;; text that's been sent.
         (with-temp-file "/tmp/preview"
           (insert text))
         (elnode-wiki-send httpcon "/tmp/preview" path))))))

(defun elnode-wikiserver-test ()
  "Test whether we should serve Wiki or not."
  (featurep 'creole))

;;;###autoload
(defun elnode-wikiserver (httpcon)
  "Serve Wiki pages from `elnode-wikiserver-wikiroot'.

HTTPCON is the request.

The Wiki server is only available if the `creole' package is
provided. Otherwise it will just error."
  (if (elnode-wikiserver-test)
      (elnode-wiki-handler httpcon elnode-wikiserver-wikiroot)
    (elnode-send-500 httpcon "The Emacs feature 'creole is required.")))


(provide 'elnode-wiki)

;;; elnode-wiki.el ends here
