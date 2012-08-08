;;; elnode-org.el

(require 'elnode)

(defun elnode-org-handler (httpcon)
  (elnode-docroot-for "~/work/org"
      with org-file
      on httpcon
      do (with-current-buffer (find-file-noselect org-file)
           (let ((org-html
                  ;; This might throw errors so you could condition-case it
                  (org-export-as-html 3 nil nil 'string)))
             (elnode-send-html httpcon org-html)))))

(defun elnode-org-update-handler (httpcon)
  "Elnode handler to do org-mode updates.

Specify `file-name' for the file to update, `node-match' for an
org-agenda match, `in-node-match' to specify what will be
replaced in the node matched org line, `replace-match' for the
replacement."
  (elnode-method httpcon
      (POST
       (let* ((file-name (elnode-http-param httpcon "file-name"))
              (node-match (elnode-http-param httpcon "node-match"))
              (in-node-match (elnode-http-param httpcon "in-node-match"))
              (replace-match (elnode-http-param httpcon "replace-match")))
         (elnode-org--update
          file-name
          node-match
          in-node-match
          replace-match)))))

(defun elnode-org--update (file-name node-match in-node-match replace-match)
  "Update org-mode specified FILE-NAME.

NODE-MATCH specifies a match expression in the manner of org-agenda views.

IN-NODE-MATCH specifies a string match expression used with the
bounds of the matched node line.

REPLACE-MATCH specifies the replacement for the IN-NODE-MATCH."
  (with-current-buffer (find-file-noselect file-name)
    (org-map-entries
     (lambda ()
       (replace-regexp
        in-node-match
        replace-match
        nil
        (point)
        (line-end-position)))
     node-match)))

(elnode-start 'elnode-org-handler :port 8002)


;;; elnode-org.el ends here
