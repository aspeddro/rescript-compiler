// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: "Eq",
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
}

var v = {
  contents: 0
};

function Make(U) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return U;
}

function f(param) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return List;
}

eq("File \"global_module_alias_test.res\", line 48, characters 12-19", List.length({
          hd: 1,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }), 2);

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

v.contents = v.contents + 1 | 0;

var H = List;

eq("File \"global_module_alias_test.res\", line 52, characters 12-19", v.contents, 12);

function g(param) {
  return List.length({
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: /* [] */0
                  }
                }
              }
            });
}

function xx(param) {
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  v.contents = v.contents + 1 | 0;
  return List;
}

eq("File \"global_module_alias_test.res\", line 80, characters 12-19", g(undefined), 4);

var V = xx(undefined);

eq("File \"global_module_alias_test.res\", line 84, characters 5-12", Curry._1(V.length, {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }), 3);

eq("File \"global_module_alias_test.res\", line 85, characters 5-12", v.contents, 15);

var H$1 = f(undefined);

eq("File \"global_module_alias_test.res\", line 87, characters 5-12", Curry._1(H$1.length, {
          hd: 1,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }), 2);

eq("File \"global_module_alias_test.res\", line 88, characters 5-12", v.contents, 21);

Mt.from_pair_suites("Global_module_alias_test", suites.contents);

var A;

var B;

var C;

var D;

var E;

var F;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.A = A;
exports.B = B;
exports.C = C;
exports.D = D;
exports.E = E;
exports.F = F;
exports.v = v;
exports.Make = Make;
exports.f = f;
exports.H = H;
exports.g = g;
exports.xx = xx;
/*  Not a pure module */
