'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");
var Process                 = require("process");
var Assert                  = require("assert");
var Curry                   = require("../../lib/js/curry");
var Path                    = require("path");
var $$Array                 = require("../../lib/js/array");
var List                    = require("../../lib/js/list");

function is_mocha() {
  var match = $$Array.to_list(Process.argv);
  if (match) {
    var exec = Path.basename(match[0]);
    if (exec === "mocha") {
      return /* true */1;
    }
    else {
      return +(exec === "_mocha");
    }
  }
  else {
    throw [
          Caml_builtin_exceptions.match_failure,
          [
            "mt.ml",
            30,
            2
          ]
        ];
  }
}

function from_suites(name, suite) {
  var match = $$Array.to_list(Process.argv);
  if (match && is_mocha(/* () */0)) {
    describe(name, function () {
          return List.iter(function (param) {
                      it(param[0], param[1]);
                      return /* () */0;
                    }, suite);
        });
    return /* () */0;
  }
  else {
    return /* () */0;
  }
}

function close_enough(x, y) {
  return +(Math.abs(x - y) < 0.0000001);
}

function from_pair_suites(name, suites) {
  var match = $$Array.to_list(Process.argv);
  if (match && is_mocha(/* () */0)) {
    describe(name, function () {
          return List.iter(function (param) {
                      var code = param[1];
                      it(param[0], function () {
                            var match = Curry._1(code, /* () */0);
                            switch (match.tag | 0) {
                              case 0 : 
                                  Assert.deepEqual(match[0], match[1]);
                                  return /* () */0;
                              case 1 : 
                                  Assert.notDeepEqual(match[0], match[1]);
                                  return /* () */0;
                              case 2 : 
                                  if (close_enough(match[0], match[1])) {
                                    return 0;
                                  }
                                  else {
                                    throw [
                                          Caml_builtin_exceptions.assert_failure,
                                          [
                                            "mt.ml",
                                            71,
                                            20
                                          ]
                                        ];
                                  }
                                  break;
                              case 3 : 
                                  Assert.throws(match[0]);
                                  return /* () */0;
                              
                            }
                          });
                      return /* () */0;
                    }, suites);
        });
    return /* () */0;
  }
  else {
    return /* () */0;
  }
}

exports.from_suites      = from_suites;
exports.from_pair_suites = from_pair_suites;
/* process Not a pure module */
