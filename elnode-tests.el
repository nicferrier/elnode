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

(defmacro should-equal (a b)
  "Simple shortcut for `(should (equal a b))'."
  `(should
    (equal ,a ,b)))

(defmacro should-match (regex a)
  "Simple shortcut for a `string-match' with `should'."
  `(should
   (string-match
    ,regex
    ,a)))

(ert-deftest elnode-join ()
  "Test the path joining."
  (should
   (equal "/la/la/file"
          (elnode-join "/la" "la" "file")))
  (should
   (equal "/la/la/file/"
          (elnode-join "/la" "la" "file/")))
  (should
   (equal "/la/la/file/"
          (elnode-join "/la" "la/file/" ""))))

(ert-deftest elnode-url-encode-path ()
  "Test the path encoding."
  (should
   (equal
    "/path/the%20path"
    (elnode-url-encode-path "/path/the path")))
  (should
   (equal
    "/path/the%20path/"
    (elnode-url-encode-path "/path/the path/")))
  (should
   (equal
    "/path/the%20%27path%27"
    (elnode-url-encode-path "/path/the 'path'"))))

(defun elnode--log-buffer-read-text (buffer)
  "Turn the buffer into a list of text.

Strips off the date format from each text line.  Primarily this
is just a test helper."
  (let* ((log-line-regex "[0-9]\\{14\\}: \\(.*\\)")
         (lines
          (split-string
           (with-current-buffer buffer
             (buffer-substring (point-min) (point-max)))
           "\n")))
    (loop for line in lines
          if (string-match log-line-regex line)
          collect (match-string 1 line))))

(ert-deftest elnode-log-buffer-log ()
  "Test the log buffer stuff."
  (let ((tf (make-temp-file "logbufferlog")))
    (with-temp-buffer
      (elnode-log-buffer-log "test it" (current-buffer) tf)
      (should
       (equal
        (marker-position elnode-log-buffer-position-written)
        (point-max)))
      (elnode-log-buffer-log "test again" (current-buffer) tf)
      (should
       (equal
        '("test it" "test again")
        (elnode--log-buffer-read-text (current-buffer)))))
    ;; Test that we can read it back from the file.
    (let* ((log-buf (find-file-noselect tf)))
      (should
       (equal
        '("test it" "test again")
        (elnode--log-buffer-read-text log-buf))))))

(ert-deftest elnode-log-buffer-log-truncates ()
  "Test the log buffer gets truncated stuff."
  (let ((log-line-regex "[0-9]\\{14\\}: \\(.*\\)")
        (tf (make-temp-file "logbufferlog"))
        (elnode-log-buffer-max-size 8))
    (with-temp-buffer
      (elnode-log-buffer-log "test it" (current-buffer) tf)
      (elnode-log-buffer-log "test again" (current-buffer) tf)
      (elnode-log-buffer-log "test three" (current-buffer) tf)
      (elnode-log-buffer-log "test four" (current-buffer) tf)
      (elnode-log-buffer-log "test five" (current-buffer) tf)
      (elnode-log-buffer-log "test six" (current-buffer) tf)
      (elnode-log-buffer-log "test seven" (current-buffer) tf)
      (elnode-log-buffer-log "test eight" (current-buffer) tf)
      (elnode-log-buffer-log "test nine" (current-buffer) tf)
      (elnode-log-buffer-log "test ten" (current-buffer) tf)
      (should
       (equal
        8
        (length
         (loop for i in
               (split-string
                (buffer-substring
                 (point-min)
                 (point-max))
                "\n")
               if (not (equal i ""))
               collect i)))))))

(ert-deftest elnode-test-error-log ()
  (let ((err-message "whoops!! something went wrong! %s" )
        (err-include "some included value"))
    (with-temp-buffer
      (let ((test-log-buf (current-buffer)))
        ;; Setup a fake server log buffer
        (flet ((elnode--get-error-log-buffer ()
                 test-log-buf))
          (elnode-error err-message err-include))
        ;; Assert the message sent to the log buffer is correctly formatted.
        (should (string-match
                 (format
                  "^.*: %s\n$"
                  (apply 'format `(,err-message ,@(list err-include))))
                 (buffer-substring (point-min) (point-max))))))))

(ert-deftest elnode-test-error-log-list ()
  (let ((err-message "whoops!! something went wrong! %s %s")
        (err-include '("included value 1" "included value 2")))
    (with-temp-buffer
      (let ((test-log-buf (current-buffer)))
        ;; Setup a fake server log buffer
        (flet ((elnode--get-error-log-buffer ()
                 test-log-buf))
          (elnode-error
           err-message
           "included value 1" "included value 2"))
        ;; Assert the message sent to the log buffer is correctly formatted.
        (should (string-match
                 (format
                  "^.*: %s\n$"
                  (apply 'format `(,err-message ,@err-include)))
                 (buffer-substring (point-min) (point-max))))))))

(ert-deftest elnode-test-access-log ()
  "Test the access logging."
  (fakir-mock-process
    :httpcon
    ((:buffer
      (elnode--http-make-hdr
       'get "/"
       '(host . "localhost")
       '(user-agent . "test-agent")))
     (:elnode-httpresponse-status 200)
     (:elnode-bytes-written 2048))
    (should
     (equal
      'done
      (catch 'elnode-parse-http
        (elnode--http-parse :httpcon))))
    (let* ((logname "ert-test")
           (buffername (format "*%s-elnode-access*" logname)))
      (flet ((elnode--log-filename
              (log-name)
              (make-temp-file "elnode-access")))
        (unwind-protect
            (progn
              (elnode-log-access logname :httpcon)
              (should
               (string-match
                (concat  "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                         "-[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}:[ ]+"
                         "200[ ]+2048[ ]+GET[ ]+/$")
                (with-current-buffer buffername
                  (buffer-substring (point-min)(point-max))))))
          (kill-buffer buffername))))))

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
    :httpcon
    ((:buffer
      (elnode--http-make-hdr
       'get "/"
       '(host . "localhost")
       '(user-agent . "test-agent"))))
    ;; Parse the header
    (should
     (equal 'done
            (catch 'elnode-parse-http
              (elnode--http-parse :httpcon))))
    ;; Now check the side effects
    (should
     (equal
      (process-get :httpcon :elnode-http-header)
      '(("Host" . "localhost")
        ("User-Agent" . "test-agent"))))))

(ert-deftest elnode--http-parse-header-incomplete ()
  "Test the HTTP parsing of an incomplete header.

An HTTP request with an incomplete header is setup and tested,
then we finish the request (fill out the header) and then test
again."
  (fakir-mock-process
    :httpcon
    ((:buffer
      "GET / HTTP/1.1\r\nHost: localh"))
    ;; Now parse
    (should
     ;; It fails with incomplete 'header signal
     (equal 'header
            (catch 'elnode-parse-http
              (elnode--http-parse :httpcon))))
    ;; Now put the rest of the header in the buffer
    (with-current-buffer (process-buffer :httpcon)
      (goto-char (point-max))
      (insert "ost\r\n\r\n"))
    (should
     ;; Now it succeeds with the 'done signal
     (equal 'done
            (catch 'elnode-parse-http
              (elnode--http-parse :httpcon))))))


(ert-deftest elnode--http-parse-body-incomplete ()
  "Tests the HTTP parsing of an incomplete body.

An HTTP request with an incomplete body is setup and tested, then
we finish the request (fill out the content to content-length)
and then test again."
  (let ((hdr
         (elnode--http-make-hdr
          'get "/"
          '(host . "localhost")
          '(user-agent . "test-agent")
          `(content-length . ,(format "%d" (length "this is not finished")))
          '(body . "this is not fin"))))
    (fakir-mock-process
        :httpcon
        ((:buffer hdr))
        ;; Now parse
        (should
         (equal 'content
                (catch 'elnode-parse-http
                  (elnode--http-parse :httpcon))))
      ;; Now put the rest of the text in the buffer
      (with-current-buffer (process-buffer :httpcon)
        (goto-char (point-max))
        (insert "ished"))
      ;; And test again
      (should
       (equal 'done
              (catch 'elnode-parse-http
                (elnode--http-parse :httpcon)))))))


(ert-deftest elnode-http-start ()
  "Test starting a response.

Especially tests the mix of header setting techniques."
  (fakir-mock-process
    :httpcon
    ()
    (elnode-http-header-set :httpcon "Content-Type" "text/html")
    (elnode-http-header-set :httpcon "Accept" "application/javascript")
    (elnode-http-start :httpcon 200 '("Content-Type" . "text/plain"))
    ;; Test that we have the correct text in the fake process buffer
    (with-current-buffer (process-buffer :httpcon)
      (goto-char (point-min))
      (should
       (re-search-forward "^Content-Type: text/html\r\n" nil t))
      (goto-char (point-min))
      (should
       (re-search-forward "^Accept: application/javascript\r\n" nil t)))))


(defun elnode-test-handler (httpcon)
  "A simple handler for testing `elnode-test-call'.

The text spat out is tested, so is the status."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (message "in special test handler")
  (elnode-http-return
   httpcon
   "<html><body><h1>Hello World</h1></body></html>"))

(ert-deftest elnode-test-call-page ()
  (with-elnode-mock-server
    ;; Test dispatcher
    (lambda (httpcon)
      (elnode-hostpath-dispatcher
       httpcon
       '(("[^/]+//test/.*" . elnode-test-handler))))
    (let ((r (elnode-test-call "/test/test.something")))
      (should
       (equal
        200
        (plist-get r :status)))
      (should
       (equal
        "<html><body><h1>Hello World</h1></body></html>"
        (plist-get r :result-string))))))

(ert-deftest elnode-http-header ()
  "Test that we have headers."
  (fakir-mock-process
    :httpcon
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
              (elnode--http-parse :httpcon))))
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
  ;; First test no cookie header
  (fakir-mock-process
    :httpcon
    ((:elnode-http-header
      '(("Referer" . "http://somehost.example/com"))))
    (should-not
     (elnode-http-cookie :httpcon "username")))
  ;; Now do we have a cookie?
  (fakir-mock-process
    :httpcon
    ((:elnode-http-header
      '(("Cookie" . "csrf=213u21321321412nsfnwlv; username=nicferrier"))))
    (should
     (equal
      "((\"username\" . \"nicferrier\"))\n"
      (pp-to-string (elnode-http-cookie :httpcon "username"))))
    (should
     (equal
      "nicferrier"
      (cdr (assoc-string
            "username"
            (elnode-http-cookie :httpcon "username")))))))

(ert-deftest elnode-test-cookie-list ()
  "Test that a cookie list property is set on the connection.

Cookie lists are good fake up values for higher abstraction
testing code so we specifically test that they work."
  (fakir-mock-process
    :httpcon
    ;; Define a cookie with a faked cookie list
    ((:elnode-http-cookie-list '(("name" . "value"))))
    (should
     (equal
      '("name" . "value")
      (elnode-http-cookie :httpcon "name"))))
  ;; Not sure about what the property should contain here...
  (fakir-mock-process
    :httpcon
    ((:elnode-http-header
      '(("Cookie" . "name=value"))))
    (elnode-http-cookie :httpcon "name")
    (should
     (equal
      ;; ... is this really right? a list of a list of a list???
      '((("name" . "value")))
      (process-get :httpcon :elnode-http-cookie-list)))))

(ert-deftest elnode-http-cookie-make ()
  "Test the cookie header maker."
  ;; Expiry using a string date
  (should
   (equal
    '("Set-Cookie" . "mycookie=101; Expires=Mon, Feb 27 2012 22:10:21 GMT;")
    (elnode-http-cookie-make
     "mycookie" 101
     :expiry "Mon, Feb 27 2012 22:10:21 GMT"))))

(ert-deftest elnode-test-http-get-params ()
  "Test that the params are ok if they are on the status line.

Sets ':elnode-http-params' to nil to trigger `elnode-http-params'
parsing. That checks the ':elnode-http-method':

- for GET it returns the parsed ':elnode-http-query'

- for POST it returns the merger of the parsed POST body and
  ':elnode-http-query'.

*** WARNING:: This test so far only handles GET ***"
  (fakir-mock-process
    :httpcon
    (:elnode-http-params
     (:elnode-http-method "GET")
     (:elnode-http-query "a=10"))
    (should (equal "10" (elnode-http-param :httpcon "a"))))
  ;; Test some more complex params
  (fakir-mock-process
    :httpcon
    (:elnode-http-params
     (:elnode-http-method "GET")
     (:elnode-http-query "a=10&b=lah+dee+dah&c+a=blah+blah"))
    (should (equal "lah dee dah" (elnode-http-param :httpcon "b")))
    (should (equal "blah blah" (elnode-http-param :httpcon "c a")))))

(ert-deftest elnode-test-http-post-params ()
  "Test that the params are ok if they are in the body.

Does a full http parse of a dummy buffer."
  (let ((httpcon :httpcon))
    (let ((post-body "a=10&b=20&c=this+is+finished"))
      (fakir-mock-process
	  :httpcon
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
                    (elnode--http-parse httpcon))))
        ;; Now test some params
        (should (equal "10" (elnode-http-param httpcon "a")))
        (should (equal "20" (elnode-http-param httpcon "b")))
        (should (equal "this is finished" (elnode-http-param httpcon "c")))))
    ;; Test get of params that aren't there
    (fakir-mock-process
      :httpcon
      ((:buffer
	(elnode--http-make-hdr
	 'post "/"
	 '(host . "localhost")
	 '(user-agent . "test-agent")
	 `(content-length . "0")
	 `(body . ""))))
      ;; Now parse
      (should
       (equal 'done
	      (catch 'elnode-parse-http
		(elnode--http-parse httpcon))))
      (should-not (elnode-http-param httpcon "a"))
      (should-not (elnode-http-param httpcon "b"))
      (should-not (elnode-http-param httpcon "c")))))

(ert-deftest elnode-test-http-post-empty-params ()
  "Test that the params are ok if they are just empty in the body."
  (let ((post-body ""))
    (fakir-mock-process
      :httpcon
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
                (elnode--http-parse :httpcon))))
      ;; Now test some params
      (should-not (elnode-http-param :httpcon "a")))))


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

(ert-deftest elnode-http-header-set ()
  "Test the premature setting of HTTP headers."
  (fakir-mock-process
    :httpcon
    ()
    (should
     (equal nil
	    (process-get :httpcon :elnode-headers-to-set)))
    (elnode-http-header-set :httpcon "Content-Type" "text/html")
    (elnode-http-header-set
     :httpcon
     (elnode-http-cookie-make "mycookie" "value"))
    (should
       (equal '(("Content-Type" . "text/html")
                ("Set-Cookie" . "mycookie=value;"))
              (process-get :httpcon :elnode-headers-to-set)))))



(ert-deftest elnode-send-json ()
  "Test sending JSON."
  (let ((httpcon :fake)
        (sent-data ""))
    (should
     (equal
      ["a string in a list"]
      (json-read-from-string
       (flet ((elnode-http-return (con data)
                (setq sent-data data)))
         (fakir-mock-process
	  httpcon
          ()
          (elnode-send-json httpcon (list "a string in a list")))
         sent-data))))))

(ert-deftest elnode--buffer-template ()
  "Test the buffer templating."
  (let ((result
         (with-temp-buffer
           (insert "<!doctype html>
<head>
  <meta charset='utf-8'>
  <meta http-equiv='X-UA-Compatible' content='IE=edge,chrome=1'>
  <title><!##E title E##!></title>
  <meta name='viewport' content='width=device-width'>
  <link rel='stylesheet' href='/talk/stuff/css/style.css'/>
  <link rel='stylesheet' href='/talk/stuff/css/basic.css'/>
</head>
<body>
    <div id='header'>
        <ul>
            <li><a href='javascript:;'>talk to a friend</a></li>
            <li><a href='/about/'>about</a></li>
            <li><a href='/blog/'>blog</a></li>
        </ul>
        <!##E name-html E##!>
    </div>
</body>
</html>")
           (elnode--buffer-template
            (current-buffer)
            '(("title" . "My webpage")
              ("name-html" . "<div>you are talking to Caroline</div>"))))))
    (should
     (string-match ".*<title>My webpage</title>.*" result))
    (should
     (string-match ".*<div>you are talking to Caroline</div>.*" result))))


(ert-deftest elnode--mapper-find ()
  "Test the mapper find function."
  (fakir-mock-process
   :httpcon
   ((:nothing))
   (should
    (equal
     (elnode--mapper-find
      :httpcon
      "localhost//wiki/somefile.creole"
      '(("[^/]+//wiki/\\(.*\\)" . elnode-wikiserver)
        ("[^/]+//.*" . elnode-webserver)))
     'elnode-wikiserver))
   (should
    (equal
     (elnode-http-mapping :httpcon)
     "localhost//wiki/somefile.creole"))
   (should
    (equal
     (elnode-http-mapping :httpcon 1)
     "somefile.creole"))
   (should
    (equal
     (elnode--mapper-find
      :httpcon
      "anyhost//wiki/somefile.creole"
      '(("[^/]+//wiki/\\(.*\\)" . elnode-wikiserver)
        ("[^/]+//.*" . elnode-webserver)))
     'elnode-wikiserver))))

(ert-deftest elnode--strip-leading-slash ()
  "Test slash stripping.

That sounds more fun than it is."
  (should
   (equal "blah"
          (elnode--strip-leading-slash "/blah")))
  (should
   (equal "blah"
          (elnode--strip-leading-slash "blah")))
  (should
   (equal "blah/"
          (elnode--strip-leading-slash "/blah/")))
  (should
   (equal "blah/"
          (elnode--strip-leading-slash "blah/"))))

(ert-deftest elnode-get-targetfile ()
  "Test the target file resolution stuff."
  (fakir-mock-process
    :httpcon
    ((:elnode-http-pathinfo "/wiki/index.creole"))
    (should
     (equal
      'elnode-wikiserver
      (elnode--mapper-find
       :httpcon
       "localhost//wiki/index.creole"
       '(("[^/]+//wiki/\\(.*\\)" . elnode-wikiserver)
         ("[^/]+//\\(.*\\)" . elnode-webserver)))))
    (fakir-mock-file (fakir-file
                      :filename "index.creole"
                      :directory "/home/elnode/wiki")
      (should
       (equal
        (elnode-get-targetfile :httpcon "/home/elnode/wiki")
        "/home/elnode/wiki/index.creole"))))
  ;; Now alter the mapping to NOT declare the mapped part...
  (fakir-mock-process
      :httpcon
    ((:elnode-http-pathinfo "/blah/thing.txt"))
    ;; ... the mapper-find should still work...
    (should
     (equal
      'elnode-webserver
      (elnode--mapper-find
       :httpcon
       "localhost//blah/thing.txt"
       '(("[^/]+//.*" . elnode-webserver)))))
    ;; ... but now there is no mapping so it only maps because of path-info
    (fakir-mock-file (fakir-file
                      :filename "thing.txt"
                      :directory "/home/elnode/www/blah")
        (should
         (equal
          (elnode-get-targetfile :httpcon "/home/elnode/www")
          "/home/elnode/www/blah/thing.txt"))))
  ;; Test without a mapping
  (fakir-mock-process
      :httpcon
      ((:elnode-http-pathinfo "/index.creole"))
    (fakir-mock-file (fakir-file
                      :filename "index.creole"
                      :directory "/home/elnode/wiki")
      (should
       (equal
        (elnode-get-targetfile :httpcon "/home/elnode/wiki")
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
    (flet ((elnode-http-method (http-con)
             "GET"))
      (elnode-method httpcon
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
	  :httpcon
          ((:elnode-http-header-syms
            '((if-modified-since . "Mon, Feb 27 2012 22:10:24 GMT"))))
          (should
           (elnode-cached-p :httpcon "/home/elnode/wiki/page.creole")))
      ;; Test the case where there is no header
      (fakir-mock-process
	:httpcon
        ((:elnode-http-header-syms
          '((user-agent . "Elnode test client"))))
        (should-not
         (elnode-cached-p :httpcon "/home/elnode/wiki/page.creole")))))

(ert-deftest elnode-docroot-for ()
  "Test the docroot protection macro."
  (let ((httpcon :fake))
    (flet ((elnode-send-404 (httpcon)
             (throw :test 404))
           (elnode-send-status (httpcon status &optional msg)
             (throw :test status))
           (send-200 (httpcon)
             (throw :test 200)))
      ;; Test straight through
      (should
       (equal
        200
        (catch :test
          (fakir-mock-process
	    :fake
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
	    :fake
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
	    :fake
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

(ert-deftest elnode-webserver ()
  (with-elnode-mock-server
    ;; The dispatcher function
    (lambda (httpcon)
      (elnode-hostpath-dispatcher
       httpcon
       '(("[^/]+/\\(.*\\)" . elnode-webserver))))
    ;; Now the actual test
    (fakir-mock-file
     (fakir-file
      :filename "blah.html"
      :directory elnode-webserver-docroot-default
      :content "<html>Fake HTML file</html>")
     (unwind-protect
         ;; Ensure the webserver uses Emacs to open files so fakir can
         ;; override it.
         (let* ((elnode-webserver-visit-file t)
                (elnode--do-error-logging nil)
                (elnode--do-access-logging-on-dispatch nil)
                (r (elnode-test-call "/blah.html")))
           (elnode-error "result -> %s" r)
           (should
            (equal
             (plist-get r :status)
             200))
           (should
            (equal
             "<html>Fake HTML file</html>"
             (plist-get r :result-string))))
       ;; Now kill the buffer that was opened to serve the file.
       (if (get-buffer "blah.html")
           (kill-buffer "blah.html"))))))


(ert-deftest elnode-client-with-stdout ()
  "Test the stdout macro.

Test that we get the right chunked encoding stuff going on."
  (with-temp-buffer
    (let ((process :fake)
          (test-buffer (current-buffer)))
      (fakir-mock-process :fake ((:elnode-http-started t))
          (progn
            (set-process-buffer process test-buffer)
            (with-stdout-to-elnode process
                (princ "hello!")))
          (should
           (equal
            (let ((str "hello!"))
              (format "%d\r\n%s\r\n0\r\n\r\n" (length str) str))
            (buffer-substring (point-min) (point-max))))))))


(ert-deftest elnode--wrap-handler ()
  "Test wrapping a handler held in a symbol with another."
  (let ((wrapped-handler-counter 0)
        (wrapping-handler-counter 0))
    ;; Define the handler we'll wrap
    (flet ((wrap-test-handler (httpcon)
             (incf wrapped-handler-counter)))
      (unwind-protect
           (progn
             ;; Wrap that handler
             (elnode--wrap-handler
              'wrap-test-handler
              "testit/$"
              (lambda (httpcon)
                (incf wrapping-handler-counter)))
             (with-elnode-mock-server 'wrap-test-handler
                 (let ((r (elnode-test-call "/something/")))
                   (should (equal wrapped-handler-counter 1))
                   (should (equal wrapping-handler-counter 0)))
               (let ((r (elnode-test-call "/testit/")))
                 (should (equal wrapped-handler-counter 1))
                 (should (equal wrapping-handler-counter 1)))))
        ;; We need to clear the symbol and it's plist whenever the
        ;; test runs, otherwise we keep state in the symbol.
        (setplist 'wrap-test-handler nil)
        (unintern 'wrap-test-handler)))))


;; Elnode auth tests

(defun elnode--auth-init-user-db (user-alist)
  "Initialize the auth database.

USER-ALIST is an assoc list of username and passwords."
  (loop for pair in user-alist
       do
       (elnode-db-put
        (car pair)
        (elnode--auth-make-hash
         (car pair)
         (cdr pair))
        elnode-auth-db)))

(ert-deftest elnode-auth-user-p ()
  "Check the authentication check.

This tests the authentication database check."
  (let ((elnode-auth-db (elnode-db-make '(elnode-db-hash))))
    ;; The only time we really need clear text passwords is when
    ;; faking records for test
    (elnode--auth-init-user-db '(("nferrier" . "password")
                                 ("someuser" . "secret")))
    (should
     (elnode-auth-user-p "someuser" "secret"))))

(ert-deftest elnode-auth-check-p ()
  "Test basic login.

Tess that we can login a user and then assert that they are
authenticated."
  (let ((elnode-loggedin-db (make-hash-table :test 'equal))
        (elnode-auth-db (elnode-db-make '(elnode-db-hash))))
    ;; The only time we really need clear text passwords is when
    ;; faking records for test
    (elnode--auth-init-user-db '(("nferrier" . "password")
                                 ("someuser" "secret")))
    ;; Test a failure
    (should
     (equal "an error occured!"
            (condition-case credentials
                (elnode-auth-login "nferrier" "secret")
              (elnode-auth
               "an error occured!"))))

    ;; Now test
    (let ((hash (elnode-auth-login "nferrier" "password")))
      (should (elnode-auth-check-p "nferrier" hash)))))

(ert-deftest elnode-auth-cookie-check-p ()
  "Check that a cookie can be used for auth."
  (let ((elnode-loggedin-db (make-hash-table :test 'equal))
        (elnode-auth-db (elnode-db-make '(elnode-db-hash))))
    ;; The only time we really need clear text passwords is when
    ;; faking records for test
    (elnode--auth-init-user-db '(("nferrier" . "password")
                                 ("someuser" "secret")))
    ;; Now test
    (let ((hash (elnode-auth-login "nferrier" "password")))
      (fakir-mock-process
	:httpcon
	((:elnode-http-header
	  `(("Cookie" . ,(concat "elnode-auth=nferrier::" hash)))))
	(should (elnode-auth-cookie-check-p :httpcon))))
    ;; Test what happens without a cookie
    (let ((hash (elnode-auth-login "nferrier" "password")))
      (fakir-mock-process
	:httpcon
	((:elnode-http-header
	  `(("Referer" . "http://somehost.example.com"))))
	(should-not
	 (condition-case token
	     (elnode-auth-cookie-check-p :httpcon)
	   (elnode-auth-token (cdr token))))))))

(ert-deftest elnode-auth-login-sender ()
  "Low levelish test of the login page sender."
  (fakir-mock-process
    :httpcon
    ()
    (elnode-auth-login-sender :httpcon "/login/" "/myapp/loggedin")
    (with-current-buffer (process-buffer :httpcon)
      (goto-char (point-min))
      (should (re-search-forward
               "<form method='POST' action='/login/'>" nil 't))
      (should (re-search-forward
               "<input type='hidden' name='redirect' value='/myapp/loggedin'/>"
               nil 't)))))

(ert-deftest elnode--auth-define-scheme-do-wrap ()
  "Test wrapper destructuring.

The function under test uses some slightly exotic destructuring
so we test it deliberately here."
  ;; First test with a specified path
  (flet ((elnode--wrap-handler (wrap-target path wrapping-func)
           (should
            (equal
             wrap-target
             'blah))
           (should
            (equal
             path
             "/doit/"))))
    (elnode--auth-define-scheme-do-wrap
     (list
      'blah
      (lambda (httpcon)
        (elnode-send-500 httpcon))
      "/doit/")))
  ;; Now with the default path
  (flet ((elnode--wrap-handler (wrap-target path wrapping-func)
           (should
            (equal
             wrap-target
             'blah))
           (should
            (equal
             path
             "/login/"))))
    (elnode--auth-define-scheme-do-wrap
     (list
      'blah
      (lambda (httpcon)
        (elnode-send-500 httpcon))))))

(ert-deftest elnode-with-auth ()
  "Test protection of code with authentication.

This tests that the auth protection macro does its job, including
the wrapping of a specified handler with the login sender."
  ;; Setup the user db
  (let ((elnode-auth-db (elnode-db-make '(elnode-db-hash))))
    ;; The only time we really need clear text passwords is when
    ;; faking records for test
    (elnode--auth-init-user-db '(("nferrier" . "password")
                                 ("someuser" . "secret")))
    ;; Setup handlers to wrap
    (flet
        ((auth-reqd-handler (httpcon)
           (elnode-with-auth httpcon 'test-auth
             (elnode-dispatcher
              httpcon
              '(("^/someplace/.*" .
                 (lambda (httpcon)
                   (elnode-send-html
                    httpcon
                    "<html><body>You are logged in!</body></html>")))
                ("^/$" .
                 (lambda (httpcon)
                   (elnode-send-html
                    httpcon
                    "<html><body>You are logged in too!</body></html>"))))))))
      ;; Make the auth scheme - since we use fset in
      ;; elnode--wrap-handler this should work ok.
      (elnode-auth-define-scheme
       'test-auth
       :test :cookie
       :cookie-name "secret"
       :redirect (elnode-auth-make-login-wrapper
                  'auth-reqd-handler
                  :target "/my-login/"))
      ;; Test that we are redirected to login when we don't have cookie
      (with-elnode-mock-server 'auth-reqd-handler
          (let ((r (elnode-test-call "/")))
            (should-equal 302 (plist-get r :status))
            (should-equal "/my-login/?to=/"
                          (assoc-default "Location" (plist-get r :header))))
        ;; Test that we get the login page - this tests that the main
        ;; handler was wrapped
        (let ((r (elnode-test-call "/my-login/")))
          (should-equal 200 (plist-get r :status))
          (should-match
           "<input type='hidden' name='redirect' value='/'/>"
           (plist-get r :result-string)))
        ;; Do it all again with a different 'to'
        (let ((r (elnode-test-call "/somepage/test/")))
          (should-equal 302 (plist-get r :status))
          (should-equal "/my-login/?to=/somepage/test/"
                        (assoc-default "Location" (plist-get r :header))))
        ;; Test that we get the login page - this tests that the main
        ;; handler was wrapped
        (let ((r (elnode-test-call "/my-login/")))
          (should-equal 200 (plist-get r :status))
          (should-match
           "<input type='hidden' name='redirect' value='/'/>"
           (plist-get r :result-string)))))))

(ert-deftest elnode-with-auth-bad-auth ()
  "Test bad auth causes login page again."
  ;; Setup the user db
  (let ((elnode-auth-db (elnode-db-make '(elnode-db-hash))))
    ;; The only time we really need clear text passwords is when
    ;; faking records for test
    (elnode--auth-init-user-db '(("nferrier" . "password")
                                 ("someuser" . "secret")))
    ;; Setup handlers to wrap
    (flet
        ((auth-reqd-handler (httpcon)
           (elnode-with-auth httpcon 'test-auth
             (elnode-dispatcher
              httpcon
              '(("^/someplace/.*" .
                 (lambda (httpcon)
                   (elnode-send-html
                    httpcon
                    "<html><body>You are logged in!</body></html>")))
                ("^/$" .
                 (lambda (httpcon)
                   (elnode-send-html
                    httpcon
                    "<html><body>You are logged in too!</body></html>"))))))))
      (elnode-auth-define-scheme
       'test-auth
       :test :cookie
       :cookie-name "secret"
       :redirect (elnode-auth-make-login-wrapper
                  'auth-reqd-handler
                  :target "/my-login/"))
      ;; Test that we are redirected to login when we don't have cookie
      (with-elnode-mock-server 'auth-reqd-handler
          ;; Test a bad auth
          (let ((r (elnode-test-call
                    "/my-login/?redirect=/somepage/test/"
                    :method 'POST
                    :parameters
                    '(("username" . "nferrier")
                      ("password" . "secret")))))
            (should-equal 302 (plist-get r :status))
            (should-equal "/my-login/?redirect=/somepage/test/"
                          (assoc-default "Location" (plist-get r :header))))))))

(provide 'elnode-tests)

;;; elnode-tests.el ends here
