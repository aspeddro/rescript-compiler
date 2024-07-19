// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let $$Set = require("../../lib/js/set.js");
let Caml = require("../../lib/js/caml.js");
let Curry = require("../../lib/js/curry.js");
let Caml_module = require("../../lib/js/caml_module.js");

let A = Caml_module.init_mod([
  "rec_module_opt.res",
  15,
  4
], {
  TAG: "Module",
  _0: [[
      "Function",
      "compare"
    ]]
});

let ASet = $$Set.Make(A);

function compare(t1, t2) {
  if (t1.TAG === "Leaf") {
    if (t2.TAG === "Leaf") {
      return Caml.string_compare(t1._0, t2._0);
    } else {
      return 1;
    }
  } else if (t2.TAG === "Leaf") {
    return -1;
  } else {
    return Curry._2(ASet.compare, t1._0, t2._0);
  }
}

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Function",
      "compare"
    ]]
}, A, {
  compare: compare
});

let X0 = {};

let Y0 = {};

let X1 = Caml_module.init_mod([
  "rec_module_opt.res",
  44,
  19
], {
  TAG: "Module",
  _0: [[
      "Function",
      "f"
    ]]
});

let Y1 = Caml_module.init_mod([
  "rec_module_opt.res",
  47,
  12
], {
  TAG: "Module",
  _0: [[
      "Function",
      "f"
    ]]
});

function f(x) {
  return x + 1 | 0;
}

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Function",
      "f"
    ]]
}, X1, {
  f: f
});

function f$1(x) {
  return x + 2 | 0;
}

Caml_module.update_mod({
  TAG: "Module",
  _0: [[
      "Function",
      "f"
    ]]
}, Y1, {
  f: f$1
});

let X;

exports.A = A;
exports.ASet = ASet;
exports.X = X;
exports.X0 = X0;
exports.Y0 = Y0;
exports.X1 = X1;
exports.Y1 = Y1;
/* A Not a pure module */
