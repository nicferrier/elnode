

(defvar elnode-server-socket nil
  "Where we store the server socket

We only keep one server for all the elnode processes. Clearly,
one improvement would be to make this abstracted so you could
create many.
")

(defun elnode-sentinel (process status)
  "Sentinel function for the main server and for the client sockets"
  (cond
   ;; Server status
   ((and 
     (equal process elnode-server-socket)
     (equal status "deleted\n"))
    (message "elnode server stopped"))

   ;; Client socket status
   ((equal status "connection broken by remote peer\n")
    (kill-buffer (process-buffer process))
    (message "elnode connection dropped"))

   ((equal status "open\n") ;; this says "open from ..."
    (message "elnode opened new connection"))

   ;; Default
   (t
    (message "wtf? %s %s" process status))
   ))

(defun elnode-log (server con msg)
  "Don't need to do anything because the sentinel should have been assigned"
  (message "elnode-log: %s" msg)
  )

(defun elnode-start ()
  "Start the elnode server.

Most of the work done by the server is actually done by
functions, the sentinel function, the log function and a filter
function (tho we don't use that yet).
"
  (interactive)
  (setq elnode-server-socket 
        (let ((buf (get-buffer-create "*elnode-webserver*")))
          (make-network-process 
           :name "*elnode-webserver-proc*"
           :buffer buf
           ;;:type 'seqpacket
           :server t
           ;; The following are not needed if we specify :local t
           :host 'local
           :service 8022
           :family 'ipv4
           ;;:local addr
           :nowait 't
           :sentinel 'elnode-sentinel
           :log 'elnode-log
           )
          ))
  )

(defun elnode-stop ()
  "Stop the elnode server"
  (interactive)
  (delete-process elnode-server-socket)
  )

;; End
