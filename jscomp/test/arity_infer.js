// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Curry = require("../../lib/js/curry.js");

function f0(x) {
  let tmp;
  if (x > 3) {
    tmp = (function (x) {
      return x + 1 | 0;
    });
  } else {
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  }
  return tmp(3);
}

function f1(x) {
  throw new Error("Not_found", {
        cause: {
          RE_EXN_ID: "Not_found"
        }
      });
  return Curry._1(undefined, x);
}

function f3(x) {
  let tmp;
  switch (x) {
    case 0 :
        tmp = (function (x) {
          return x + 1 | 0;
        });
        break;
    case 1 :
        tmp = (function (x) {
          return x + 2 | 0;
        });
        break;
    case 2 :
        tmp = (function (x) {
          return x + 3 | 0;
        });
        break;
    case 3 :
        tmp = (function (x) {
          return x + 4 | 0;
        });
        break;
    default:
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
  }
  return tmp(3);
}

exports.f0 = f0;
exports.f1 = f1;
exports.f3 = f3;
/* No side effect */
