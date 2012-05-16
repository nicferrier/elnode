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

which are stored as symbols the same as the normal header keys."
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
       if (string-match
           "^\\([A-Za-z0-9.-]+\\):[ ]*\\(.*\\)"
           line)
       do
         (let ((name (intern (downcase (match-string 1 line))))
               (value (match-string 2 line)))
           (puthash name value header-hash)))
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

(defun elnode-client--last-5 (existing new)
  "Return the last 5 characters of EXISTING and NEW combined."
  (let ((full (concat existing new)))
    (cond
      ((> (length full) 5)
       (substring full (- (length full) 5)))
      (t
       full))))

(ert-deftest elnode-client--last-5 ()
  "Unit test the last-5."
  (should
   (equal "AB0AB"
          (elnode-client--last-5
           (elnode-client--last-5 "AB0" "A") "B"))))

(defun elnode-client-chunked-end-p (connection data)
  "Has the CONNECTION with DATA got the last chunk?

A side effect of this is that the CONNECTION has it's `:last5'
property updated with the current last-5 characters.

Use this function if you are writing `stream' handlers with
`elnode-client-post'.  It can be used to detect and of stream and
close the connection approp."
  (let* ((last5 (process-get connection :last5))
         (newlast5 (elnode-client--last-5 last5 data)))
    (process-put connection :last5 newlast5)
    (string-match "\r\n0\r\n$" newlast5)))

(ert-deftest elnode-client-chunked-end-p ()
  (let* ((data "0\r\n")
         :fakecon)
    (fakir-mock-process ((:last5 "blah blah\r\n"))
        (should (elnode-client--chunked-end-p :fakecon data)))
    (fakir-mock-process ((:last5 "blah blah\r"))
        (should-not (elnode-client--chunked-end-p :fakecon data)))))

(defun elnode--http-post-filter (con data callback mode)
  "Filter function for HTTP POST.

Not actually a filter function because it also receives a
CALLBACK from the actual filter function which is a lexical
closure inside `elnode-http-post'.

CALLBACK is a user supplied function handling the return from the
HTTP server.

MODE comes from the `elnode-http-post' call.  This function
handles the MODE by either streaming the data to the CALLBACK or
by collecting it and then batching it to the CALLBACK."
  (with-current-buffer (process-buffer con)
    (let ((header (process-get con :http-header)))
      (if (and header (eq mode 'stream))
          (funcall callback con header data)
          (save-excursion
            (goto-char (point-max))
            (insert data)
            ;; Find the header if we don't have it
            (if (and (not header)
                     (progn
                       (goto-char (point-min))
                       (re-search-forward "\r\n\r\n" nil t)))
              (let ((hdr (elnode--http-client-header-parse
                          (buffer-substring (point-min) (point-max))))
                    ;; From the point of the end of header to the end
                    ;; is the data we need... this may be nothing.
                    (part-data (if (> (point-max) (point))
                                   (buffer-substring (point) (point-max))
                                   "")))
                (process-put con :http-header-pos (point))
                (process-put con :http-header hdr)
                ;; If we're in stream mode do CALLBACK straight away
                (when (eq mode 'stream)
                  (funcall callback con hdr part-data)))
              ;; We have the header and we're in batch mode... check
              ;; whether the response is ended
              (let ((response-complete-data
                     (cond
                       ((equal "chunked" (gethash 'transfer-encoding header))
                        ;; Do we have the last chunk?
                        (if (progn
                              (goto-char (point-max))
                              (re-search-backward "\r\n0\r\n$" nil t))
                            ;; This simply gets rid of the chunked
                            ;; encoding.... ideally we'd validate it.
                            (replace-regexp-in-string
                             "\r\n[0-9]+\r\n"
                             ""
                             (buffer-substring
                              (process-get con :http-header-pos) (point-max)))))
                       ((gethash 'content-length header)
                        (when (>= (string-to-number
                                   (gethash 'content-length header))
                                  (-
                                   (point-max)
                                   (process-get con :http-header-pos)))
                          ;; If we have a content length then we're
                          ;; ended when we have that much data
                          ;;
                          ;; NOTE this only works because the chunked
                          ;; test comes first, we COULD be using
                          ;; chunked encoding AND content-length.
                          (buffer-substring
                           (process-get con :http-header-pos)
                           (point-max)))))))
                (when response-complete-data
                  (funcall callback con header response-complete-data)
                  (delete-process con)))))))))

(ert-deftest elnode-client-http-post-filter ()
  "Test the filter in streaming mode."
  (let* (cb-hdr
         cd-data
         deleted
         (con :fake)
         (cb (lambda (con hdr data)
               (unless cb-hdr
                 (setq cb-hdr hdr)
                 (setq cb-data data))
               (when (elnode-client-chunked-end-p con data)
                 (delete-process con)))))
    (flet ((delete-process (proc)
             (setq deleted t)))
      (fakir-mock-process ((:buffer "HTTP/1.1 200\r
Host: hostname\r
Transfer-encoding: chunked\r\n"))
          (progn
            (should-not cb-hdr)
            (elnode--http-post-filter con "\r\n" cb 'stream)
            (should cb-hdr)
            (should-not deleted)
            ;; Some header tests
            (should
             (equal "hostname"
                    (gethash 'host cb-hdr)))
            (should
             (equal "200"
                    (gethash 'status-code cb-hdr)))
            (should
             (equal "1.1"
                    (gethash 'status-version cb-hdr)))
            ;; Now see if we can send data through the stream api
            (elnode--http-post-filter con "11\r\nhello world" cb 'stream)
            (should-not deleted)
            (elnode--http-post-filter con "\r\n0\r\n" cb 'stream))))))

(ert-deftest elnode-client-http-post-filter-batch-mode-content-length ()
  "Test the filter in batch mode."
  (let* (cb-hdr
         cd-data
         deleted
         (con :fake)
         (cb (lambda (con hdr data)
               (setq cb-hdr hdr)
               (setq cb-data data))))
    ;; We need to flet delete-process to do nothing
    (flet ((delete-process (proc)
             (setq deleted t)))
      (fakir-mock-process ((:buffer "HTTP/1.1 200\r
Host: hostname\r
Content-length: 11\r\n"))
          (progn
            (should-not cb-hdr)
            (elnode--http-post-filter con "\r\n" cb 'batch)
            (should-not cb-hdr)
            (elnode--http-post-filter con "hello world" cb 'batch)
            (should cb-hdr)
            (should deleted)
            (should
             (equal "hostname"
                    (gethash 'host cb-hdr)))
            (should
             (equal "200"
                    (gethash 'status-code cb-hdr)))
            (should
             (equal "1.1"
                    (gethash 'status-version cb-hdr))))))))

(ert-deftest elnode-client-http-post-filter-batch-mode-chunked ()
  "Test the filter in batch mode."
  (let* (cb-hdr
         cd-data
         deleted
         (con :fake)
         (cb (lambda (con hdr data)
               (setq cb-hdr hdr)
               (setq cb-data data))))
    ;; We need to flet delete-process to do nothing
    (flet ((delete-process (proc)
             (setq deleted t)))
      (fakir-mock-process ((:buffer "HTTP/1.1 200\r
Transfer-encoding: chunked\r
Host: hostname\r\n"))
          (progn
            (should-not cb-hdr)
            (elnode--http-post-filter con "\r\n" cb 'batch)
            (should-not cb-hdr)
            (elnode--http-post-filter con "11\r\nhello world" cb 'batch)
            (should-not cb-hdr)
            (elnode--http-post-filter con "\r\n0\r\n" cb 'batch)
            (should cb-hdr)
            (should deleted)
            (should
             (equal "hostname"
                    (gethash 'host cb-hdr)))
            (should
             (equal "200"
                    (gethash 'status-code cb-hdr)))
            (should
             (equal "1.1"
                    (gethash 'status-version cb-hdr))))))))

(defun elnode-http-post (host port path data type callback &optional mode)
  "Make an HTTP POST to the HOST on PORT with PATH and send DATA.

DATA is of MimeType TYPE.

When the request comes back the CALLBACK is called.

MODE defines what it means for the request to cause the CALLBACK
to be fired.  When MODE is `stream' then the CALLBACK is called
for every batch of data received after the header has arrived.
This allows streaming data to somewhere else; hence `stream'
mode.

The default MODE is `batch' which collects all the data from the
response before calling CALLBACK with the header and all the
data."
  (let* ((mode (or mode 'batch))
         (dest (format "%s:%s/%s" host port path))
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
       (let ((mode mode)
             (cb callback))
         (elnode--http-post-filter con data cb mode))))
    ;; Send the POST
    (let ((submission (format "POST %s HTTP/1.1\r
Host: %s\r
Content-type: %s\r
Content-length:%d\r
\r
%s" path host type (length data) data)))
          (process-send-string con submission))
    con))

;;; elnode-client.el ends here
