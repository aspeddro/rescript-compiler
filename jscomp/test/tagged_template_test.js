// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Mt = require("./mt.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Tagged_template_libJs = require("./tagged_template_lib.js");

var query = Tagged_template_libJs.sql`SELECT * FROM ${"users"} WHERE id = ${"5"}`;

var length = Tagged_template_libJs.length`hello ${10} what's the total length? Is it ${3}?`;

function foo(strings, values) {
  var res = "";
  var valueCount = values.length;
  for(var i = 0; i < valueCount; ++i){
    res = res + Caml_array.get(strings, i) + String(Math.imul(Caml_array.get(values, i), 10));
  }
  return res + Caml_array.get(strings, valueCount);
}

var res = foo([
      "| 5 * 10 = ",
      " |"
    ], [5]);

Mt.from_pair_suites("tagged templates", {
      hd: [
        "with externals, it should return a string with the correct interpolations",
        (function (param) {
            return {
                    TAG: "Eq",
                    _0: query,
                    _1: "SELECT * FROM 'users' WHERE id = '5'"
                  };
          })
      ],
      tl: {
        hd: [
          "with externals, it should return the result of the function",
          (function (param) {
              return {
                      TAG: "Eq",
                      _0: length,
                      _1: 52
                    };
            })
        ],
        tl: {
          hd: [
            "with rescript function, it should return a string with the correct interpolations",
            (function (param) {
                return {
                        TAG: "Eq",
                        _0: res,
                        _1: "| 5 * 10 = 50 |"
                      };
              })
          ],
          tl: {
            hd: [
              "a template literal tagged with json should generate a regular string interpolation for now",
              (function (param) {
                  return {
                          TAG: "Eq",
                          _0: "some random " + "string",
                          _1: "some random string"
                        };
                })
            ],
            tl: /* [] */0
          }
        }
      }
    });

/* query Not a pure module */
