// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Pervasives = require("../../lib/js/pervasives.js");

let ff = Pervasives.string_of_float;

function f(v) {
  return String(v);
}

Mt.from_pair_suites("To_string_test", {
  hd: [
    "File \"to_string_test.res\", line 6, characters 8-15",
    (function () {
      return {
        TAG: "Eq",
        _0: Pervasives.string_of_float(Pervasives.infinity),
        _1: "inf"
      };
    })
  ],
  tl: {
    hd: [
      "File \"to_string_test.res\", line 6, characters 49-56",
      (function () {
        return {
          TAG: "Eq",
          _0: Pervasives.string_of_float(Pervasives.neg_infinity),
          _1: "-inf"
        };
      })
    ],
    tl: /* [] */0
  }
});

exports.ff = ff;
exports.f = f;
/*  Not a pure module */
