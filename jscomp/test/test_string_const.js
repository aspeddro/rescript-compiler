// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml_string = require("../../lib/js/caml_string.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var f = Caml_string.get("ghsogh", 3);

var hh;

try {
  hh = Caml_string.get("ghsogh", -3);
}
catch (raw_e){
  var e = Caml_js_exceptions.internalToOCamlException(raw_e);
  if (e.RE_EXN_ID === "Invalid_argument") {
    console.log(e._1);
    hh = /* 'a' */97;
  } else {
    throw e;
  }
}

exports.f = f;
exports.hh = hh;
/* hh Not a pure module */
