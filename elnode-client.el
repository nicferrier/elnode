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

(defun elnode-client--last-7 (existing new)
  "Return the last 7 characters of EXISTING and NEW combined."
  (let ((full (concat existing new)))
    (cond
      ((> (length full) 7)
       (substring full (- (length full) 7)))
      (t
       full))))

(ert-deftest elnode-client--last-7 ()
  "Unit test the last-7."
  (should
   (equal "AB0ABAB"
          (elnode-client--last-7
           (elnode-client--last-7 "AB0AB" "A") "B"))))

(defun elnode-client-chunked-end-p (connection data)
  "Has the CONNECTION with DATA got the last chunk?

A side effect of this is that the CONNECTION has it's `:last5'
property updated with the current last-5 characters.

Use this function if you are writing `stream' handlers with
`elnode-client-post'.  It can be used to detect and of stream and
close the connection approp."
  (let* ((last7 (process-get connection :last7))
         (newlast7 (elnode-client--last-7 last7 data)))
    (process-put connection :last7 newlast7)
    (string-match "\r\n0\r\n\r\n$" newlast7)))

(ert-deftest elnode-client-chunked-end-p ()
  (let* ((data "0\r\n\r\n")
         :fakecon)
    (fakir-mock-process ((:last7 "blah blah\r\n"))
        (should (elnode-client-chunked-end-p :fakecon data)))
    (fakir-mock-process ((:last7 "blah blah\r"))
        (should-not (elnode-client-chunked-end-p :fakecon data)))))

(defun elnode-client--chunked-decode-stream (con data consumer)
  "Decode the chunked encoding stream on the process CON.

DATA is a lump of data from the stream, as passed from a filter
function for example.

CONSUMER is a function that will be called with the resulting
data like:

  CON CHUNK

the CON is the same as the CON in this call.  The `chunk' is the
chunk that has been read.  Only complete chunks are sent to the
CONSUMER.

When the chunked stream ends the CONSUMER is called with CHUNK
being `:done'.  This can be used to do clean up.  It is NOT
expected that the callback will have to clean up the CON, that
should be done by the caller.

CON is used to store state with the process property
`:chunked-encoding-buffer' being used as a buffer."
  ;; Make data the whole chunk
  (setq data (let ((saved (process-get con :chunked-encoding-buffer)))
               (if saved (concat saved data) data)))
  (if (not (string-match "^\\([0-9A-Fa-f]+\\)\r\n" data))
      (process-put con :chunked-encoding-buffer data)
      ;; We have identified a chunk
      (let* ((chunk-num (match-string 1 data))
             (chunk-size (string-to-number chunk-num 16))
             (toread-pos (+ 2 (length chunk-num))) ; +2 == \r\n after chunk sz
             (chunk-end (+ toread-pos chunk-size)))
        (if (< (length data) (+ 2 chunk-end)) ; +2 == \r\n at end of chunk
            (process-put con :chunked-encoding-buffer data)
            (let ((toread (substring data toread-pos chunk-end))
                  (trailing (substring data chunk-end (+ chunk-end 2)))
                  (left (substring data (+ chunk-end 2))))
              (if trailing
                  (assert (equal trailing "\r\n") t))
              (cond
                ((equal 0 chunk-size)
                 ;; Finished
                 (funcall consumer con :done)
                 :done)
                ((> chunk-size (length toread))
                 (process-put con :chunked-encoding-buffer data))
                (t
                 ;; Eat the data
                 (funcall consumer con toread)
                 ;; Clear the buffer
                 (process-put con :chunked-encoding-buffer "")
                 ;; Go round again if we need to
                 (if left
                     (elnode-client--chunked-decode-stream
                      con left consumer)))))))))

(ert-deftest elnode-client--chunked-decode-stream ()
  "Test the chunked decoding."
  ;; Test incomplete chunk delivered (missing trailing crlf)
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process ()
          (progn
            (should-not
             (equal
              :done
              (elnode-client--chunked-decode-stream
               proc "b\r\nhello world" 'consumer)))
            (should
             (equal "b\r\nhello world"
                    (process-get proc :chunked-encoding-buffer)))
            (should
             (equal
              :done
              (elnode-client--chunked-decode-stream
               proc "\r\n0\r\n\r\n" 'consumer)))))))
  ;; Test incomplete chunk packet delivered
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process ()
          (progn
            (should-not
             (equal
              :done
              (elnode-client--chunked-decode-stream
               proc "b\r\nhello wor" 'consumer)))
            (should
             (equal "b\r\nhello wor"
                    (process-get proc :chunked-encoding-buffer)))))))
  ;; Test more than 1 complete chunk delivered
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process ()
          (progn
            (should
             (equal :done
                    (elnode-client--chunked-decode-stream
                     proc
                     "6\r\nhello!\r\nb\r\nhello world\r\n0\r\n\r\n"
                     'consumer)))
            (should
             (equal "hello!hello world" res))))))
  ;; Test one call handling one chunk and then the end
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process ()
          (progn
            (should
             (equal :done
                    (elnode-client--chunked-decode-stream
                     proc "5\r\nhello\r\n0\r\n\r\n" 'consumer)))
            (should
             (equal "hello" res)))))))

(defun elnode--http-post-filter (con data callback mode)
  "Filter function for HTTP POST.

Not actually a filter function because it also receives the
CALLBACK and the MODE from the actual filter function, a lexical
closure inside `elnode-http-post'.

CALLBACK is a user supplied function handling the return from the
HTTP server.

MODE comes from the `elnode-http-post' call.  This function
handles the MODE by either streaming the data to the CALLBACK or
by collecting it and then batching it to the CALLBACK."
  (with-current-buffer (process-buffer con)
    (let ((header (process-get con :http-header)))
      (if (not header)
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
                                     nil)))
                  (process-put con :http-header-pos (point))
                  (process-put con :http-header hdr)
                  ;; If we have more data call ourselves to process it
                  (when part-data
                    (elnode--http-post-filter con part-data callback mode)))))
          ;; We have the header, read the body and call callback
          (cond
            ((equal "chunked" (gethash 'transfer-encoding header))
             (elnode-client--chunked-decode-stream
              con data
              ;; FIXME we still need the callback to know if this is completion
              (lambda (con data)
                (cond
                  ((eq mode 'stream)
                   (funcall callback con header data)
                   (when (eq data :done)
                     (delete-process con)))
                  ((and (eq mode 'batch)
                        (eq data :done))
                   (funcall callback con header
                            (process-get con :elnode-client-buffer))
                   (delete-process con))
                  (t
                   (process-put
                    con :elnode-client-buffer
                    (concat (or (process-get con :elnode-client-buffer) "")
                            data)))))))
            ;; We have a content-length header so just buffer that much data
            ((gethash 'content-length header)
             (let ((so-far (process-get con :elnode-client-buffer)))
               (if (< (string-to-number (gethash 'content-length header))
                      (length so-far))
                   (process-put
                    con :elnode-client-buffer
                    (concat so-far data))
                   ;; We have all the data, callback and then kill the process
                   (funcall callback con header so-far)
                   (delete-process con)))))))))

(ert-deftest elnode-client-http-post-filter ()
  "Test the filter in streaming mode."
  (let* (cb-hdr
         cd-data
         deleted
         (con :fake)
         (callback (lambda (con hdr data)
                     (unless cb-hdr
                       (setq cb-hdr hdr))
                     (unless (eq data :done)
                       (setq cb-data data)))))
    (flet ((delete-process (proc)
             (setq deleted t)))
      (fakir-mock-process ((:buffer "HTTP/1.1 200\r
Host: hostname\r
Transfer-encoding: chunked\r\n"))
          (progn
            (should-not cb-hdr)
            (elnode--http-post-filter con "\r\n" callback 'stream)
            ;; Because there is no data yet the header is not set
            (should-not cb-hdr)
            ;; Now send a valid chunk through the stream api
            (elnode--http-post-filter
             con "b\r\nhello world\r\n" callback 'stream)
            (should cb-hdr)
            (should-not deleted)
            (should (equal cb-data "hello world"))
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
            ;; Now send the final one
            (elnode--http-post-filter con "0\r\n\r\n" callback 'stream)
            (should (equal cb-data "hello world"))
            (should deleted))))))

(ert-deftest elnode-client-http-post-filter-batch-mode-content-length ()
  "Test the filter in batch mode with fixed content-length."
  (let* (cb-hdr
         cd-data
         deleted
         (con :fake)
         (callback (lambda (con hdr data)
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
            (elnode--http-post-filter con "\r\n" callback 'batch)
            (should-not cb-hdr)
            (elnode--http-post-filter con "hello world" callback 'batch)
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
  "Test the filter in batch mode with chunked encoding."
  (let* (cb-hdr
         cb-data
         deleted
         (con :fake)
         (callback (lambda (con hdr data)
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
            (elnode--http-post-filter con "\r\n" callback 'batch)
            (should-not cb-hdr)
            (elnode--http-post-filter con "b\r\nhello world" callback 'batch)
            (should-not cb-hdr)
            (should-not cb-data)
            (elnode--http-post-filter con "\r\n0\r\n\r\n" callback 'batch)
            (should cb-hdr)
            (should (equal "hello world" cb-data))
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

(defun elnode-client-http-post (host
                                port
                                path
                                data
                                type
                                callback &optional mode)
  "Make an HTTP POST to the HOST on PORT with PATH and send DATA.

DATA is of MimeType TYPE.

When the request comes back the CALLBACK is called.

MODE defines what it means for the request to cause the CALLBACK
to be fired.  When MODE is `stream' then the CALLBACK is called
for every chunk of data received after the header has arrived.
This allows streaming data to somewhere else; hence `stream'
mode.

The default MODE is `batch' which collects all the data from the
response before calling CALLBACK with the header and all the
data."
  (let* ((mode (or mode 'batch))
         (dest (format "%s:%s/%s" host port path))
         (buf (generate-new-buffer dest))
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

(defun elnode-client--load-path-ize (lisp)
  "Wrap LISP in the current load-path."
  (concat
   ;; There is a very strange thing with sending lisp to
   ;; (read) over a piped stream... (read) can't cope with
   ;; multiple lines; so we encode newline here.
   ;;(replace-regexp-in-string
   ;; "\n"
   ;; "\\\\n"
   (format "(progn (setq load-path (quote %S)) %s)"
           (append (list default-directory) load-path)
           lisp)))

(defun elnode-test-handler (httpcon)
  "Test handler for running in child emacs."
  (elnode-http-start httpcon "200" '("Content-type" . "text/html"))
  (elnode-http-return httpcon "hello world"))

(require 'loadhist)

(defvar elnode-client--remote-handlers
  (make-hash-table :test 'equal)
  "A hash table of established child Emacs' running handlers.")

(defun elnode-client--remote-handlers-kill ()
  "Empty the remote handlers list."
  (interactive)
  (setq elnode-client--remote-handlers (make-hash-table :test 'equal)))

(defun elnode-client--handler-mapper-client (con hdr data httpcon)
  "HTTP client callback helper for the mapper."
  (unless (process-get httpcon :header-sent)
    (elnode-http-start httpcon
                       (gethash 'status-code hdr))
    (process-put httpcon :header-sent t))
  (if (not (elnode-client-chunked-end-p con data))
      (elnode-http-send-string httpcon data)
      ;; Else we return and delete the con coz we finished
      (elnode-http-return httpcon data)
      (delete-process con)))

(defun elnode-client--handler-mapper (httpcon port)
  "Elnode handler helper to call the HTTP server on PORT."
  (elnode-client-http-post
   "localhost"
   port
   "/" "" "application/x-elnode"
   (lambda (con hdr data)
     (elnode-client--handler-mapper-client con hdr data httpcon))
   'stream))

(defun elnode-client--handler-lisp (handler port to-require)
  "Return a file with Lisp to start HANDLER on PORT.

Used by `elnode-client-handler' to construct the lisp to send.
You're unlikely to need to override this at all, the function is
just here to make the implementation easier to debug.

TO-REQUIRE is a list of things to require, currently only 1 is
allowed."
  (let ((temp-file
         (make-temp-file
          (format "elnodeclient-%s" (symbol-name handler)))))
    (with-temp-file temp-file
      (insert
       (elnode-client--load-path-ize
        (format "(progn
 (setq elnode-do-init nil)
 (setq elnode--do-error-logging nil)
 (require (quote %s))
 (elnode-start (quote %s) :port %d)
 (while t (sleep-for 60)))"
                to-require
                (symbol-name handler)
                port))))
    temp-file))

(defun elnode-client-handler (handler)
  "Map access to an elnode HANDLER in a child emacs.

Spawn a child Emacs with Lisp code to load the file for the
specified handler and start it being served by an Elnode server.

Returns a function which will call the handler over HTTP."
  (let* ((handler-file (symbol-file handler))
         (handler-provide '(elnode-client)) ; (file-provides handler-file))
         (ephemeral-port 9001)
         (proc-buffer (get-buffer-create
                       (format "* %s *" (symbol-name handler))))
         (emacsrun
          (format
           "emacs -q  -batch -l %s"
           (elnode-client--handler-lisp handler
                                        ephemeral-port
                                        (car handler-provide))))
         (proc
          (start-process-shell-command "elnode-client" proc-buffer emacsrun)))
    ;; Store the new server
    (puthash ephemeral-port proc elnode-client--remote-handlers)
    ;; Make a handler to call the server
    (process-put
     proc :handler
     (lambda (httpcon)
       (elnode-client--handler-mapper httpcon ephemeral-port)))
    (process-get proc :handler)))

(defun elnode-client-call-handler (httpcon port handler)
  "Call the handler serving PORT with HANDLER.

The process serving HANDLER is get or created."
  (flet ((get-or-create-process (handler)
           (let ((proc (gethash port elnode-client--remote-handlers)))
             (if proc
                 (process-get proc :handler)
                 (elnode-client-handler handler)))))
    (funcall (get-or-create-process handler) httpcon)))

(provide 'elnode-client)

;;; elnode-client.el ends here
