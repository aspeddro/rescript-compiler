// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Primitive_object = require("rescript/lib/js/primitive_object.js");
let Primitive_exceptions = require("rescript/lib/js/primitive_exceptions.js");

let v = "gso";

function is_equal() {
  if (v.codePointAt(0) !== /* 'g' */103) {
    throw {
      RE_EXN_ID: "Assert_failure",
      _1: [
        "equal_exception_test.res",
        4,
        2
      ],
      Error: new Error()
    };
  }
  
}

function is_exception() {
  try {
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === "Not_found") {
      return;
    }
    throw exn;
  }
}

function is_normal_exception(_x) {
  let A = /* @__PURE__ */Primitive_exceptions.create("A");
  let v = {
    RE_EXN_ID: A,
    _1: 3
  };
  try {
    throw v;
  } catch (raw_exn) {
    let exn = Primitive_exceptions.internalToException(raw_exn);
    if (exn.RE_EXN_ID === A) {
      if (exn._1 === 3) {
        return;
      }
      throw exn;
    }
    throw exn;
  }
}

function is_arbitrary_exception() {
  let A = /* @__PURE__ */Primitive_exceptions.create("A");
  try {
    throw {
      RE_EXN_ID: A,
      Error: new Error()
    };
  } catch (exn) {
    return;
  }
}

let suites_0 = [
  "is_equal",
  is_equal
];

let suites_1 = {
  hd: [
    "is_exception",
    is_exception
  ],
  tl: {
    hd: [
      "is_normal_exception",
      is_normal_exception
    ],
    tl: {
      hd: [
        "is_arbitrary_exception",
        is_arbitrary_exception
      ],
      tl: /* [] */0
    }
  }
};

let suites = {
  hd: suites_0,
  tl: suites_1
};

let e = {
  RE_EXN_ID: "Not_found"
};

function eq(x) {
  return x.RE_EXN_ID === "Not_found";
}

let Not_found = /* @__PURE__ */Primitive_exceptions.create("Equal_exception_test.Not_found");

if (Primitive_object.equal(e, {
    RE_EXN_ID: Not_found
  }) !== false) {
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "equal_exception_test.res",
      45,
      0
    ],
    Error: new Error()
  };
}

if (Not_found === "Not_found" !== false) {
  throw {
    RE_EXN_ID: "Assert_failure",
    _1: [
      "equal_exception_test.res",
      46,
      0
    ],
    Error: new Error()
  };
}

Mt.from_suites("exception", suites);

exports.v = v;
exports.is_equal = is_equal;
exports.is_exception = is_exception;
exports.is_normal_exception = is_normal_exception;
exports.is_arbitrary_exception = is_arbitrary_exception;
exports.suites = suites;
exports.e = e;
exports.eq = eq;
exports.Not_found = Not_found;
/*  Not a pure module */
