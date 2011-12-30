load('env.rhino.1.2.js');
load('jsunity/jsunity-0.6.js');

suites = []

load(arguments[0]);

jsUnity.log = function (s) {
    print(s);
};

var results = jsUnity.run.apply(jsUnity, suites);
java.lang.System.exit(results.failed);


