;;; elnode-tests.el --- tests for Elnode -*- lexical-binding: t -*-

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
;; This is just the tests for Elnode.

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elnode--private-function
;;
;; for private functions.

;;; Code:

(require 'ert)
(require 'fakir)
(require 'elnode)

(ert-deftest elnode-test-error-log-list ()
  (let ((err-message "whoops!! something went wrong! %s %s")
        (err-include '("included value 1" "included value 2")))
    (with-temp-buffer
      (let ((test-log-buf (current-buffer)))
        ;; Setup a fake server log buffer
        (flet ((elnode--get-error-log-buffer () test-log-buf))
          (elnode-error
           err-message
           "included value 1" "included value 2"))
        ;; Assert the message sent to the log buffer is correctly formatted.
        (should (string-match
                 (format
                  "^elnode-.*: %s\n$"
                  (apply 'format `(,err-message ,@err-include)))
                 (buffer-substring (point-min) (point-max))))))))

(ert-deftest elnode-deferring ()
  "Testing the defer setup."
  (let* ((result :not-done)
         (httpcon :fake)
         (handler (lambda (httpcon) (setq result :done)))
         (elnode--deferred (list)))
    ;; The queue starts empty
    (should (equal 0 (length elnode--deferred)))
    ;; Then we add to it...
    (elnode--deferred-add httpcon handler)
    (should (equal 1 (length elnode--deferred)))
    ;; Then we process it...
    (elnode--deferred-processor)
    ;; ... that should have emptied it out...
    (should (eq result :done))
    (should (equal 0 (length elnode--deferred)))
    ;; Now we add a handler that defers...
    (elnode--deferred-add httpcon
                          (lambda (httpcon)
                            (elnode-defer-now handler)))
    (should (equal 1 (length elnode--deferred)))
    ;; Now we process...
    (elnode--deferred-processor)
    ;; ... should still have the deferred handler in it...
    (should (equal 1 (length elnode--deferred)))
    ;; ... process again ...
    (elnode--deferred-processor)
    (should (equal 0 (length elnode--deferred)))))


(ert-deftest elnode--make-http-hdr ()
  "Test the construction of headers"
  (should
   (equal
    (elnode--http-make-hdr
     'get "/"
     '(host . "host1")
     '(user-agent . "test-agent"))
    "GET / HTTP/1.1\r
Host: host1\r
User-Agent: test-agent\r
\r
"))
  (should
   (equal
    (elnode--http-make-hdr
     'get "/"
     '(host . "host2")
     '(user-agent . "test-agent")
     '(body . "my data"))
    "GET / HTTP/1.1\r
Host: host2\r
User-Agent: test-agent\r
\r
my data")))

(ert-deftest elnode--http-parse-header-complete ()
  "Test the HTTP parsing."
  (fakir-mock-process
    ((:buffer
      (elnode--http-make-hdr
       'get "/"
       '(host . "localhost")
       '(user-agent . "test-agent"))))
    ;; Parse the header
    (should
     (equal 'done
            (catch 'elnode-parse-http
              (elnode--http-parse nil))))
    ;; Now check the side effects
    (should
     (equal
      (process-get nil :elnode-http-header)
      '(("Host" . "localhost")
        ("User-Agent" . "test-agent"))))))

(ert-deftest elnode--http-parse-header-incomplete ()
  "Test the HTTP parsing of an incomplete header.

An HTTP request with an incomplete header is setup and tested,
then we finish the request (fill out the header) and then test
again."
  (fakir-mock-process
    ((:buffer
      "GET / HTTP/1.1\r\nHost: localh"))
    ;; Now parse
    (should
     ;; It fails with incomplete 'header signal
     (equal 'header
            (catch 'elnode-parse-http
              (elnode--http-parse nil))))
    ;; Now put the rest of the header in the buffer
    (with-current-buffer (process-buffer nil)
      (goto-char (point-max))
      (insert "ost\r\n\r\n"))
    (should
     ;; Now it succeeds with the 'done signal
     (equal 'done
            (catch 'elnode-parse-http
              (elnode--http-parse nil))))))


(ert-deftest elnode--http-parse-body-incomplete ()
  "Tests the HTTP parsing of an incomplete body.

An HTTP request with an incomplete body is setup and tested, then
we finish the request (fill out the content to content-length)
and then test again."
  (fakir-mock-process
    ((:buffer
      (elnode--http-make-hdr
       'get "/"
       '(host . "localhost")
       '(user-agent . "test-agent")
       `(content-length . ,(format "%d" (length "this is not finished")))
       '(body . "this is not fin"))))
    ;; Now parse
    (should
     (equal 'content
            (catch 'elnode-parse-http
              (elnode--http-parse nil))))
    ;; Now put the rest of the text in the buffer
    (with-current-buffer (process-buffer nil)
      (goto-char (point-max))
      (insert "ished"))
    ;; And test again
    (should
     (equal 'done
            (catch 'elnode-parse-http
              (elnode--http-parse nil))))))

(ert-deftest elnode-test-call-page ()
  (with-elnode-mock-server
    ;; Test dispatcher
    (lambda (httpcon)
      (elnode-hostpath-dispatcher
       httpcon
       '(("[^/]+/test/.*" . elnode-test-handler))))
    (let ((r (elnode-test-call "/test/test.something")))
      (should (equal (plist-get r :status) 200)))))

(ert-deftest elnode-http-header ()
  "Test that we have headers."
  (fakir-mock-process
    ((:buffer
      (elnode--http-make-hdr
       'get "/"
       '(host . "localhost")
       '(user-agent . "test-agent")
       '(if-modified-since . "Mon, Feb 27 2012 22:10:21 GMT")
       `(content-length . ,(format "%d" (length "this is finished")))
       '(body . "this is finished"))))
    ;; Now parse
    (should
     (equal 'done
            (catch 'elnode-parse-http
              (elnode--http-parse nil))))
    (should
     (equal "test-agent"
            (elnode-http-header :httpcon "User-Agent")))
    (should
     (equal "test-agent"
            (elnode-http-header :httpcon 'user-agent)))
    (should
     (equal "test-agent"
            (elnode-http-header :httpcon 'User-Agent)))
    (should
     (equal '(20299 65357)
            (elnode-http-header :httpcon 'if-modified-since :time)))
    ;; FIXME - add a test for bad time encoding
    (should
     (equal "Mon, Feb 27 2012 22:10:21 GMT"
            (elnode-http-header :httpcon 'if-modified-since)))))


(ert-deftest elnode-test-cookie ()
  "Test the cookie retrieval"
  (flet (;; Define this so that the cookie list is not retrieved
         (process-get (proc key)
           nil)
         ;; Just define it to do nothing
         (process-put (proc key data)
           nil)
         ;; Get an example cookie header
         (elnode-http-header (httpcon name)
           "csrf=213u21321321412nsfnwlv; username=nicferrier"))
    (let ((con ""))
      (should (equal
               (pp-to-string (elnode-http-cookie con "username"))
               "((\"username\" . \"nicferrier\"))\n"))
      (should (equal
               (cdr (assoc-string
                     "username"
                     (elnode-http-cookie con "username")))
               "nicferrier")))))


(ert-deftest elnode-test-http-get-params ()
  "Test that the params are ok if they are on the status line.

Sets ':elnode-http-params' to nil to trigger `elnode-http-params'
parsing. That checks the ':elnode-http-method':

- for GET it returns the parsed ':elnode-http-query'

- for POST it returns the merger of the parsed POST body and
  ':elnode-http-query'.

*** WARNING:: This test so far only handles GET ***"
  (fakir-mock-process
    (:elnode-http-params
     (:elnode-http-method "GET")
     (:elnode-http-query "a=10"))
    (should (equal "10" (elnode-http-param 't "a"))))
  ;; Test some more complex params
  (fakir-mock-process
    (:elnode-http-params
     (:elnode-http-method "GET")
     (:elnode-http-query "a=10&b=lah+dee+dah&c+a=blah+blah"))
    (should (equal "lah dee dah" (elnode-http-param 't "b")))
    (should (equal "blah blah" (elnode-http-param 't "c a")))))

(ert-deftest elnode-test-http-post-params ()
  "Test that the params are ok if they are in the body.

Does a full http parse of a dummy buffer."
  (let ((post-body "a=10&b=20&c=this+is+finished"))
    (fakir-mock-process
      ((:buffer
        (elnode--http-make-hdr
         'post "/"
         '(host . "localhost")
         '(user-agent . "test-agent")
         `(content-length . ,(format "%d" (length post-body)))
         `(body . ,post-body))))
      ;; Now parse
      (should
       (equal 'done
              (catch 'elnode-parse-http
                (elnode--http-parse nil))))
      ;; Now test some params
      (should (equal "10" (elnode-http-param nil "a")))
      (should (equal "20" (elnode-http-param nil "b")))
      (should (equal "this is finished" (elnode-http-param nil "c"))))))


(ert-deftest elnode--http-result-header ()
  "Test that we can make result headers."
  (let ((l '((content-type . "text/html"))))
    (should
     (equal
      (elnode--http-result-header l)
      "Transfer-Encoding: chunked\r
Content-Type: text/html\r
")))
  (let ((l '()))
    (should
     (equal
      (elnode--http-result-header l)
      "Transfer-Encoding: chunked\r
"))))


(ert-deftest elnode-send-json ()
  "Test sending JSON."
  (let ((httpcon :fake)
        (sent-data ""))
    (should
     (equal
      ["a string in a list"]
      (json-read-from-string
       (flet ((elnode-http-return
               (con data)
               (setq sent-data data)))
         (fakir-mock-process ()
           (elnode-send-json httpcon (list "a string in a list")))
         sent-data))))))

(ert-deftest elnode--mapper-find ()
  "Test the mapper find function."
  (fakir-mock-process
   ((:nothing))
   (should
    (equal
     (elnode--mapper-find
      :fake
      "localhost/wiki/somefile.creole"
      '(("[^/]+/wiki/\\(.*\\)" . elnode-wikiserver)
        ("[^/]+/.*" . elnode-webserver)))
     'elnode-wikiserver))
   (should
    (equal
     (elnode-http-mapping :fake)
     "localhost/wiki/somefile.creole"))
   (should
    (equal
     (elnode-http-mapping :fake 1)
     "somefile.creole"))
   (should
    (equal
     (elnode--mapper-find
      :fake
      "anyhost/wiki/somefile.creole"
      '(("[^/]+/wiki/\\(.*\\)" . elnode-wikiserver)
        ("[^/]+/.*" . elnode-webserver)))
     'elnode-wikiserver))))


(ert-deftest elnode-get-targetfile ()
  "Test the target file resolution stuff."
  (fakir-mock-process
    ((:elnode-http-pathinfo "/wiki/index.creole"))
    (should
     (equal
      (elnode--mapper-find
       :fake
       "localhost/wiki/index.creole"
       '(("[^/]+/wiki/\\(.*\\)" . elnode-wikiserver)
         ("[^/]+/.*" . elnode-webserver)))
      'elnode-wikiserver))
    (fakir-mock-file (fakir-file
                      :filename "index.creole"
                      :directory "/home/elnode/wiki")
      (should
       (equal
        (elnode-get-targetfile :fake "/home/elnode/wiki")
        "/home/elnode/wiki/index.creole")))))

(ert-deftest elnode-worker-elisp ()
  "Test the `elnode-worker-elisp' macro.

Runs some lisp in a child Emacs and tests that it outputs the
right thing."
  (let* ((bufname (generate-new-buffer-name "elnode-worker-elisp-test"))
         (buf (get-buffer-create bufname)))
    (elnode-wait-for-exit
     ;; Nice simple bit of elisp to run in the child
     (elnode-worker-elisp
         buf
         ((a 10)
          (b 20))
       (setq x a)
       (princ x)))
    (should
     (equal
      "10"
      (let ((output
             (with-current-buffer buf
               (buffer-substring (point-min) (point-max)))))
        (kill-buffer buf)
        output)))))

(ert-deftest elnode-method ()
  "A quick test for `elnode-method'."
  (let ((httpcon :fake)
        method)
    (flet ((elnode-http-method
            (http-con)
            "GET"))
      (elnode-method
        (GET
         (setq method "GET"))
        (POST
         (set method "POST")))
      (should (equal method "GET")))))


(ert-deftest elnode--under-docroot-p ()
  "Test that the docroot protection works."
  (fakir-mock-file (fakir-file
                    :filename "index.creole"
                    :directory "/home/elnode/wiki")
    (should
     (elnode--under-docroot-p
      "/home/elnode/wiki/index.creole"
      "/home/elnode/wiki"))
    (should
     (elnode--under-docroot-p
      "/home/elnode/wiki/index.creole"
      "~/wiki"))
    (should-not
     (elnode--under-docroot-p
      "/home/elnode/wiki/blah/index.creole"
      "/home/elnode/wiki"))
    (should-not
     (elnode--under-docroot-p
      "/home/elnode/wiki/blah.creole"
      "/home/elnode/wiki"))
    (should-not
     (elnode--under-docroot-p
      "/home/elnode/wikiroot/blah.creole"
      "/home/elnode/wiki"))
    (should-not
     (elnode--under-docroot-p
      "/home/elnode/wiki/blah.creole"
      "/home/elnode/wikiroot"))))

(ert-deftest elnode-cached-p ()
  "Is a resource cached?"
  (fakir-mock-file (fakir-file
                    :filename "page.creole"
                    :directory "/home/elnode/wiki"
                    :mtime "Mon, Feb 27 2012 22:10:21 GMT")
    (fakir-mock-process
      ((:elnode-http-header-syms
        '((if-modified-since . "Mon, Feb 27 2012 22:10:24 GMT"))))
      (should
       (elnode-cached-p :httpcon "/home/elnode/wiki/page.creole")))
    ;; Test the case where there is no header
    (fakir-mock-process
      ((:elnode-http-header-syms
        '((user-agent . "Elnode test client"))))
      (should-not
       (elnode-cached-p :httpcon "/home/elnode/wiki/page.creole")))))

(ert-deftest elnode-docroot-for ()
  "Test the docroot protection macro."
  (let ((httpcon :fake))
    (flet ((elnode-send-404
            (httpcon)
            (throw :test 404))
           (elnode-send-status
            (httpcon status &optional msg)
            (throw :test status))
           (send-200
            (httpcon)
            (throw :test 200)))
      ;; Test straight through
      (should
       (equal
        200
        (catch :test
          (fakir-mock-process
            ((:elnode-http-pathinfo "/wiki/test.creole")
             (:elnode-http-mapping '("/wiki/test.creole" "test.creole")))
            (fakir-mock-file
              (fakir-file
               :filename "test.creole"
               :directory "/home/elnode/wikiroot")
              (elnode-docroot-for "/home/elnode/wikiroot"
                with target-path
                on httpcon
                do
                (send-200 httpcon)))))))
      ;; Non-existant path
      (should
       (equal
        404
        (catch :test
          (fakir-mock-process
            ((:elnode-http-pathinfo "/wiki/test.creole")
             (:elnode-http-mapping '("/wiki/test.creole" "test.creole")))
            (fakir-mock-file
              (fakir-file
               :filename "test.creole"
               :directory "/home/elnode/wikiroot")
              (elnode-docroot-for "/home/elnode/wikifiles"
                with target-path
                on httpcon
                do
                (send-200 httpcon)))))))
      ;; Test the cached check
      (should
       (equal
        304
        (catch :test
          (fakir-mock-process
            ((:elnode-http-pathinfo "/wiki/test.creole")
             (:elnode-http-mapping '("/wiki/test.creole" "test.creole"))
             (:elnode-http-header-syms
              '((if-modified-since . "Mon, Feb 27 2012 22:10:24 GMT"))))
            (fakir-mock-file
              (fakir-file
               :filename "test.creole"
               :directory "/home/elnode/wikiroot"
               :mtime "Mon, Feb 27 2012 22:10:20 GMT")
              (elnode-docroot-for "/home/elnode/wikiroot"
                with target-path
                on httpcon
                do
                (send-200 httpcon))))))))))

(ert-deftest elnode-wiki-worker ()
  "Test the worker elisp.

Basically this is the same test as in the creole library but done
via a child process."
  (let* ((page "~/creole/index.creole")
         (outbuf (get-buffer-create
                  (generate-new-buffer-name
                   "elnode-worker-wiki-test"))))
    (elnode--wiki-call
     outbuf
     "= A Creole file ="
     page)
    ;; What are our assertions??
    (should
     (let ((res
            (with-current-buffer outbuf
              (goto-char (point-min))
              (re-search-forward
               "<div id='top'></div><h1>A Creole file</h1>"
               nil
               t))))
       (kill-buffer outbuf)
       res))))


(ert-deftest elnode-wiki-page ()
  "Full stack Wiki test."
  (with-elnode-mock-server
    ;; The dispatcher function
    (lambda (httpcon)
      (elnode-hostpath-dispatcher
       httpcon
       '(("[^/]+/wiki/\\(.*\\)" . elnode-wikiserver))))
    ;; Setup the the Creole file handler mocking.
    (flet
        ((elnode--worker-lisp-helper
          (child-lisp)
          `((progn
              (require 'creole)
              (require 'cl)
              (flet ((creole--get-file
                      (filename)
                      (let ((buf (get-buffer-create "wikibuf")))
                        (with-current-buffer buf
                          (insert "= A Creole file ="))
                        buf)))
                ,@child-lisp)))))
      ;; Now the actual test
      (fakir-mock-file (fakir-file
                        :filename "test.creole"
                        :directory "/home/elnode/wiki")
        (let ((r (elnode-test-call "/wiki/test.creole")))
          (elnode-error "result -> %s" r)
          (should
           (equal
            (plist-get r :status)
            200)))))))

(provide 'elnode-tests)

;;; elnode-tests.el ends here
