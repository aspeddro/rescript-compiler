// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

var v = {
  contents: 0
};

var arr = Caml_array.make(10, (function (param) {
        
      }));

function f(param) {
  var n = 0;
  while(n < 10) {
    var j = n;
    Caml_array.set(arr, j, (function(j){
        return function (param) {
          v.contents = v.contents + j | 0;
        }
        }(j)));
    n = n + 1 | 0;
  };
}

f();

$$Array.iter((function (x) {
        Curry._1(x, undefined);
      }), arr);

console.log(String(v.contents));

if (v.contents !== 45) {
  throw new Error("Assertion Failure. File: test_while_closure.res, Line: 55, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var count = 10;

exports.v = v;
exports.count = count;
exports.arr = arr;
exports.f = f;
/*  Not a pure module */
