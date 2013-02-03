// elnode-ijs.js - js for doing js shell
var elnode_ijs =
    (function() {
         var comet = function () {
             $.ajax(
                 { url: "/ijs/",
                   dataType: "json",
                   success: function (data, status) {
                       if (data) {
                           for (var key in data) {
                               var val = data[key];
                               var result;
                               try {
                                   with (window) {
                                       result = JSON.stringify(eval(val));
                                   }
                               } 
                               catch (err) {
                                   console.log("error: ", err);
                               }
                               $.post(
                                   "/ijsresult/", {
                                       uuid: key,
                                       result: result
                                   });
                           }
                       }
                       else {
                           console.log("no data!");
                       }
                   },
                   complete: function (jqXHR, status) {
                       elnode_ijs.comet();
                   }
                 }
             );
         };
         return {
             comet: comet,
             thing: 10
         };
     })();

elnode_ijs.comet();

// elnode_ijs.js ends here