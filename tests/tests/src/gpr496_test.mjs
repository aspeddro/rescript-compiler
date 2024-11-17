// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";
import * as Primitive_bool from "rescript/lib/es6/Primitive_bool.js";

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + test_id.contents.toString()),
      () => ({
        TAG: "Eq",
        _0: x,
        _1: y
      })
    ],
    tl: suites.contents
  };
}

let expected = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

let expected2 = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

let u = [
  false,
  false,
  true,
  true,
  -1,
  1,
  0,
  0
];

eq("File \"gpr496_test.res\", line 35, characters 12-19", expected, u);

eq("File \"gpr496_test.res\", line 37, characters 12-19", expected, expected2);

function ff(x, y) {
  return Primitive_bool.min(x, y());
}

eq("File \"gpr496_test.res\", line 40, characters 12-19", true < false, false);

Mt.from_pair_suites("Gpr496_test", suites.contents);

export {
  suites,
  test_id,
  eq,
  expected,
  expected2,
  u,
  ff,
}
/*  Not a pure module */
