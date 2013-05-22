// How to add commands

function elnode_bookmark (url, title) {
    var req = new XMLHttpRequest();
    var u = encodeURIComponent(url);
    var i = encodeURIComponent(title);
    var t = new Date().getTime();
    var bm_url = "http://localhost:8004/bm/save?u=" + u + "&i=" + i + "&t=" + t;
    req.open("GET", bm_url, true);
    req.onload = function (e) {
        console.log("we sent a page ok");
    };
    req.send(null);
}

chrome.commands.onCommand.addListener(
    function(command) {
        if (command == "elnode-bookmark-page") {
            chrome.tabs.query(
                {active: true, currentWindow: true},
                function (tabs) {
                    console.log(
                        "the tab has: ", 
                        { 
                            "title": tabs[0].title,
                            "url": tabs[0].url
                        }
                    );
                    elnode_bookmark(tabs[0].url, tabs[0].title);
                }
            );
        }
    }
);
