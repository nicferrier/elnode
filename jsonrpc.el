;; Expose your running emacs to the local web by using elnode
;; Copyright (C) 2010 by Nic Ferrier			
(require 'json)						
(require 'elnode)					
							
(setq exposedMethods '(				
		       ("hello" hello) 	
		       ("bye" bye) 		
		       ))			
							
(defun hello (msg)					
  "world")						
							
(defun bye (msg)					
  "bye")						
     							
(defun assocdr (key alist)				
  (cdr (assoc key alist)))				
     							
(defun assocadr (key alist)				
  (car 
   (cdr 
    (assoc key alist))))
     
(defun dispatchjson (eljson)				
  (let* (
	 (message (json-read-from-string eljson))		
	 (jsonrpc-id (assocdr 'jsonrpc message)) ; version
	 (method-name (assocdr 'method message))		
	 (params (assocdr 'params message))		
	 (id (assocdr 'id message))   			
       )
    (if (fset 'method (assocadr method-name exposedMethods))
  	(setq result (funcall 'method params))	
      (setq result "0"))	    			
    (json-encode result)		    			
    )
  )  	       	       	       	    
     				    
(defun json-handler (httpcon)	    
  "Handle a POST. 		    
     		  
If it's not a POST send a 400."
  (if (not (equal "POST" (elnode-http-method httpcon)))
      (progn	  
        (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
        (elnode-http-return httpcon (format "<html>
<head>	   	  
<body>	   	  
<form method='POST' action='%s'>
method: <input type='text' name='method' value='hello'/>
param: <input type='text' name='params' value='0'/>
<input type='submit' name='send'/><br>
gives an error only because we're not json-encoding yet
try from testjson.ahk or testjson.py
</form>	   	  
</body>	   	  
</html>	   	  
" (elnode-http-pathinfo httpcon))))
    (let* (
	   (params (elnode-http-params httpcon))
	   (eljson (car ( car params)))
	   (result "error")
	   )
      (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
      (setq global-json eljson)
      (ignore-errors (setq result (dispatchjson eljson)))
      (elnode-http-return 
       httpcon 	  
       result
       )
      )
    )
  )
  	   	  
  				      						       
(elnode-start 'json-handler 8029 "localhost")
  				      
;; End				      
  				      
  				      
  
  
  
  
  
  
  
  
  
