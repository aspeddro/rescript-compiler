// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("../../lib/js/curry.js");

var delayed = {
  contents: (function (param) {
      
    })
};

for(var i = 1; i <= 2; ++i){
  var f = (function(i){
  return function f(n, x) {
    if (x !== 0) {
      var prev = delayed.contents;
      delayed.contents = (function (param) {
          Curry._1(prev, undefined);
          f(((n + 1 | 0) + i | 0) - i | 0, x - 1 | 0);
        });
      return ;
    }
    if (i !== n) {
      throw new Error("Assertion Failure. File: gpr_858_unit2_test.res, Line: 6, Col: 13", {
                cause: {
                  RE_EXN_ID: "Assertion_failure"
                }
              });
    }
    
  }
  }(i));
  f(0, i);
}

Curry._1(delayed.contents, undefined);

/*  Not a pure module */
