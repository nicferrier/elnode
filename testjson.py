import urllib2, json

jparams = json.dumps({"jsonrpc" : "2.0", "method" : "hello",
		       "params" : 0, "id" : 1})
req = urllib2.Request("http://localhost:8029",
                      headers = {
        "Content-Type": "application/json",})
		      
f = urllib2.urlopen(req, jparams )
result = f.read()	      
print result

