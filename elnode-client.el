;;; elnode-client.el --- elnode HTTP client -*- lexical-binding: t -*-

(require 'elnode)
(eval-when-compile
  (require 'cl))
(require 'fakir)


(defun elnode--http-post-sentinel (con evt)
  "Sentinel for the HTTP POST."
  (case evt
    ("closed\n"
     (message "http post closed"))
    ("connection broken by peer\n"
     (message "http client went away"))
    (t
     (message "some message %s" evt))))

(defun elnode--http-client-header-parse (data)
  "Parse an HTTP response header.

Each header line is stored in the hash with a symbol form of the
header name.

The status line is expected to be the first line of the data.
The status is stored in the header as well with the following
keys:

  status-version
  status-code
  status-string

"
  (let* ((header-hash (make-hash-table :test 'equal))
         (header-lines (split-string data "\r\n"))
         (status-line (car header-lines)))
    (when (string-match
           "HTTP/\\([0-9.]+\\) \\([0-9]\\{3\\}\\)\\( \\(.*\\)\\)*"
           status-line)
      (puthash 'status-version (match-string 1 status-line) header-hash)
      (puthash 'status-code (match-string 2 status-line) header-hash)
      (puthash 'status-string
               (or (match-string 4 status-line) "")
               header-hash))
    (loop for line in (cdr header-lines)
       do
         (let ((m (string-match
                   "^\\([A-Za-z0-9.-]+\\):[ ]*\\(.*\\)"
                   line)))
           (when m
             (let ((name (intern (downcase (match-string 1 line))))
                   (value (match-string 2 line)))
               (puthash name value header-hash)))))
    header-hash))

(ert-deftest elnode-client-header-parse ()
  "Test HTTP header parsing."
  (let ((hdrs (elnode--http-client-header-parse
               "HTTP/1.0 200 Ok\r
Content-type: text/html\r
Content-length: 1000\r
")))
    (should (equal "1.0" (gethash 'status-version hdrs)))
    (should (equal "200" (gethash 'status-code hdrs)))
    (should (equal "Ok" (gethash 'status-string hdrs)))
    (should (equal "text/html" (gethash 'content-type hdrs)))
    (should (equal "1000" (gethash 'content-length hdrs))))
  (let ((hdrs (elnode--http-client-header-parse
               "HTTP/1.0 400\r
Content-type: text/html\r
Content-length: 1000\r
")))
    (should (equal "1.0" (gethash 'status-version hdrs)))
    (should (equal "400" (gethash 'status-code hdrs)))
    (should (equal "" (gethash 'status-string hdrs)))
    (should (equal "text/html" (gethash 'content-type hdrs)))
    (should (equal "1000" (gethash 'content-length hdrs)))))

(defun elnode--http-post-filter (con data callback)
  "Filter function for HTTP POST.

Not actually a filter function because it also receives a
CALLBACK from the actual filter function which is a lexical
closure inside `elnode-http-post'.

CALLBACK is a user supplied function handling the return from the
HTTP server."
  (with-current-buffer (process-buffer con)
    (let ((header (process-get con :http-header)))
      (if header
          (funcall callback con header data)
          (save-excursion
            (goto-char (point-max))
            (insert data)
            (goto-char (point-min))
            (when (re-search-forward "\r\n\r\n" nil t)
              (let ((hdr (elnode--http-client-header-parse
                          (buffer-substring (point-min) (point-max)))))
                (process-put con :http-header hdr)
                ;; FIXME - need to deal with data that goes beyond
                ;; header that we haven't dealt with yet
                (funcall callback con hdr data))))))))

(ert-deftest elnode-client-http-post-filter ()
  "Test the filter."
  (let* (cb-hdr
         cd-data
         (con :fake)
         (cb (lambda (con hdr data)
               (setq cb-hdr hdr)
               (setq cb-data data))))
    (fakir-mock-process ((:buffer "HTTP/1.1 200\r\nHost: hostname\r\n"))
        (progn
          (should-not cb-hdr)
          (elnode--http-post-filter con "\r\n" cb)
          (should cb-hdr)
          (should
           (equal "hostname"
                  (gethash 'host cb-hdr)))
          (should
           (equal "200"
                  (gethash 'status-code cb-hdr)))
          (should
           (equal "1.1"
                  (gethash 'status-version cb-hdr)))))))

(defun elnode-http-post (host port path data type callback)
  "Make an HTTP POST to the HOST on PORT with PATH and send DATA.

DATA is of MimeType TYPE.

When the request comes back the CALLBACK is called."
  (let* ((dest (format "%s:%s/%s" host port path))
         (buf (get-buffer-create dest))
         (con (open-network-stream
               (format "elnode-http-post-%s" dest)
               buf
               host
               port)))
    (set-process-sentinel con 'elnode--http-post-sentinel)
    (set-process-filter
     con
     (lambda (con data)
       (let ((cb callback))
         (elnode--http-post-filter con data cb))))
    ;; Send the POST
    (let ((submission (format "POST %s HTTP/1.1\r
Host: %s\r
Content-type: %s\r
Content-length:%d\r
\r
%s" path host type (length data) data)))
          (process-send-string con submission))
    con))

(defun elnode-client-test ()
  (interactive)
  (let ((host "localhost")
        (port "8003")
        (path "/blah")
        (callback (lambda (con hdr data)
                    (message "%s %s" hdr data))))
    (elnode-http-post host port path "blah" "text/plain" callback)))

(defmacro with-stdout-to-elnode (httpcon &rest body)
  "Execute BODY so that any output gets sent to HTTPCON."
  (declare (indent defun))
  (let ((hv (make-symbol "httpconvar"))
        (val (make-symbol "value")))
    `(with-temp-buffer
       (let ((,hv ,httpcon)
             (standard-output (current-buffer)))
         (let ((,val (progn ,@body)))
           ;; Need a loop here
           (elnode-http-send-string
            ,hv
            (buffer-substring (point-min) (point-max)))
           (elnode-http-return ,hv))))))

(ert-deftest elnode-client-with-stdout ()
  "Test the stdout macro.

Test that we get the right chunked encoding stuff going on."
  (with-temp-buffer
    (let ((process :fake)
          (test-buffer (current-buffer)))
      (fakir-mock-process ((:elnode-http-started t))
          (progn
            (set-process-buffer process test-buffer)
            (with-stdout-to-elnode process
                (princ "hello!")))
          (should
           (equal
            (let ((str "hello!"))
              (format "%d\r\n%s\r\n0\r\n\r\n\r\n" (length str) str))
            (buffer-substring (point-min) (point-max))))))))

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


;;; elnode-client.el ends here
