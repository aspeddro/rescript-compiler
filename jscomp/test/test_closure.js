// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let $$Array = require("../../lib/js/array.js");
let Curry = require("../../lib/js/curry.js");
let Caml_array = require("../../lib/js/caml_array.js");

let v = {
  contents: 0
};

function f() {
  let arr = Caml_array.make(10, (function (param) {
    
  }));
  for(let i = 0; i <= 9; ++i){
    Caml_array.set(arr, i, (function (param) {
      v.contents = v.contents + i | 0;
    }));
  }
  return arr;
}

let u = f();

$$Array.iter((function (x) {
  Curry._1(x, undefined);
}), u);

if (v.contents !== 45) {
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "test_closure.res",
            52,
            2
          ]
        }
      });
}

exports.v = v;
exports.f = f;
/* u Not a pure module */
