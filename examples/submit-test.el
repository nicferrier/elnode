(defun submit-test-handler (httpcon)
  (elnode-http-start httpcon 200 
                     '("Content-type" . "text/html"))
  (let ((foo (elnode-http-param httpcon "foo")))
    (elnode-http-return 
     httpcon 
     (cond 
      (foo
       (format "<html><body>Value entered: %s</body></html>" foo))
      (t "<html>
  <body>
    Enter a value:
    <form method=\"get\" action=\"/\">
     <input name=\"foo\"/>
     <input type=\"submit\"/>
    </form>
  </body>
</html>
")))))
