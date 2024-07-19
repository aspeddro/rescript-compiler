// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml_obj = require("../../lib/js/caml_obj.js");
let Caml_sys = require("../../lib/js/caml_sys.js");
let Caml_bytes = require("../../lib/js/caml_bytes.js");
let Caml_int64 = require("../../lib/js/caml_int64.js");
let Caml_format = require("../../lib/js/caml_format.js");
let Caml_string = require("../../lib/js/caml_string.js");
let Caml_exceptions = require("../../lib/js/caml_exceptions.js");
let Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

Caml_external_polyfill.resolve("register_named_value")("Pervasives.array_bound_error", {
  RE_EXN_ID: "Invalid_argument",
  _1: "index out of bounds"
});

function failwith(s) {
  throw new Error("Failure", {
        cause: {
          RE_EXN_ID: "Failure",
          _1: s
        }
      });
}

function invalid_arg(s) {
  throw new Error("Invalid_argument", {
        cause: {
          RE_EXN_ID: "Invalid_argument",
          _1: s
        }
      });
}

let Exit = /* @__PURE__ */Caml_exceptions.create("Test_per.Exit");

function min(x, y) {
  if (Caml_obj.lessequal(x, y)) {
    return x;
  } else {
    return y;
  }
}

function max(x, y) {
  if (Caml_obj.greaterequal(x, y)) {
    return x;
  } else {
    return y;
  }
}

function abs(x) {
  if (x >= 0) {
    return x;
  } else {
    return -x | 0;
  }
}

function lnot(x) {
  return x ^ -1;
}

let min_int = -2147483648;

let infinity = Caml_int64.float_of_bits([
  2146435072,
  0
]);

let neg_infinity = Caml_int64.float_of_bits([
  -1048576,
  0
]);

let nan = Caml_int64.float_of_bits([
  2146435072,
  1
]);

let max_float = Caml_int64.float_of_bits([
  2146435071,
  4294967295
]);

let min_float = Caml_int64.float_of_bits([
  1048576,
  0
]);

let epsilon_float = Caml_int64.float_of_bits([
  1018167296,
  0
]);

function $caret(s1, s2) {
  let l1 = s1.length;
  let l2 = s2.length;
  let s = Caml_bytes.create(l1 + l2 | 0);
  Caml_external_polyfill.resolve("blit_string")(s1, 0, s, 0, l1);
  Caml_external_polyfill.resolve("blit_string")(s2, 0, s, l1, l2);
  return s;
}

function char_of_int(n) {
  if (n < 0 || n > 255) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "char_of_int"
          }
        });
  }
  return n;
}

function string_of_bool(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function bool_of_string(x) {
  switch (x) {
    case "false" :
        return false;
    case "true" :
        return true;
    default:
      throw new Error("Invalid_argument", {
            cause: {
              RE_EXN_ID: "Invalid_argument",
              _1: "bool_of_string"
            }
          });
  }
}

function string_of_int(n) {
  return Caml_format.format_int("%d", n);
}

function valid_float_lexem(s) {
  let l = s.length;
  let _i = 0;
  while(true) {
    let i = _i;
    if (i >= l) {
      return $caret(s, ".");
    }
    let match = Caml_string.get(s, i);
    if (match >= 48) {
      if (match >= 58) {
        return s;
      }
      _i = i + 1 | 0;
      continue;
    }
    if (match !== 45) {
      return s;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function string_of_float(f) {
  return valid_float_lexem(Caml_format.format_float("%.12g", f));
}

function $at(l1, l2) {
  if (l1) {
    return {
      hd: l1.hd,
      tl: $at(l1.tl, l2)
    };
  } else {
    return l2;
  }
}

function open_out_gen(mode, perm, name) {
  return Caml_external_polyfill.resolve("ml_open_descriptor_out")(Caml_external_polyfill.resolve("sys_open")(name, mode, perm));
}

function open_out(name) {
  return open_out_gen({
    hd: "Open_wronly",
    tl: {
      hd: "Open_creat",
      tl: {
        hd: "Open_trunc",
        tl: {
          hd: "Open_text",
          tl: /* [] */0
        }
      }
    }
  }, 438, name);
}

function open_out_bin(name) {
  return open_out_gen({
    hd: "Open_wronly",
    tl: {
      hd: "Open_creat",
      tl: {
        hd: "Open_trunc",
        tl: {
          hd: "Open_binary",
          tl: /* [] */0
        }
      }
    }
  }, 438, name);
}

function flush_all() {
  let _x = Caml_external_polyfill.resolve("ml_out_channels_list")();
  while(true) {
    let x = _x;
    if (!x) {
      return;
    }
    try {
      Caml_external_polyfill.resolve("ml_flush")(x.hd);
    }
    catch (exn){
      
    }
    _x = x.tl;
    continue;
  };
}

function output_bytes(oc, s) {
  Caml_external_polyfill.resolve("ml_output")(oc, s, 0, s.length);
}

function output_string(oc, s) {
  Caml_external_polyfill.resolve("ml_output")(oc, s, 0, s.length);
}

function output(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "output"
          }
        });
  }
  Caml_external_polyfill.resolve("ml_output")(oc, s, ofs, len);
}

function output_substring(oc, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "output_substring"
          }
        });
  }
  Caml_external_polyfill.resolve("ml_output")(oc, s, ofs, len);
}

function output_value(chan, v) {
  Caml_external_polyfill.resolve("output_value")(chan, v, /* [] */0);
}

function close_out(oc) {
  Caml_external_polyfill.resolve("ml_flush")(oc);
  Caml_external_polyfill.resolve("ml_close_channel")(oc);
}

function close_out_noerr(oc) {
  try {
    Caml_external_polyfill.resolve("ml_flush")(oc);
  }
  catch (exn){
    
  }
  try {
    return Caml_external_polyfill.resolve("ml_close_channel")(oc);
  }
  catch (exn$1){
    return;
  }
}

function open_in_gen(mode, perm, name) {
  return Caml_external_polyfill.resolve("ml_open_descriptor_in")(Caml_external_polyfill.resolve("sys_open")(name, mode, perm));
}

function open_in(name) {
  return open_in_gen({
    hd: "Open_rdonly",
    tl: {
      hd: "Open_text",
      tl: /* [] */0
    }
  }, 0, name);
}

function open_in_bin(name) {
  return open_in_gen({
    hd: "Open_rdonly",
    tl: {
      hd: "Open_binary",
      tl: /* [] */0
    }
  }, 0, name);
}

function input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "input"
          }
        });
  }
  return Caml_external_polyfill.resolve("ml_input")(ic, s, ofs, len);
}

function unsafe_really_input(ic, s, _ofs, _len) {
  while(true) {
    let len = _len;
    let ofs = _ofs;
    if (len <= 0) {
      return;
    }
    let r = Caml_external_polyfill.resolve("ml_input")(ic, s, ofs, len);
    if (r === 0) {
      throw new Error("End_of_file", {
            cause: {
              RE_EXN_ID: "End_of_file"
            }
          });
    }
    _len = len - r | 0;
    _ofs = ofs + r | 0;
    continue;
  };
}

function really_input(ic, s, ofs, len) {
  if (ofs < 0 || len < 0 || ofs > (s.length - len | 0)) {
    throw new Error("Invalid_argument", {
          cause: {
            RE_EXN_ID: "Invalid_argument",
            _1: "really_input"
          }
        });
  }
  unsafe_really_input(ic, s, ofs, len);
}

function really_input_string(ic, len) {
  let s = Caml_bytes.create(len);
  really_input(ic, s, 0, len);
  return s;
}

function input_line(chan) {
  let build_result = function (buf, _pos, _x) {
    while(true) {
      let x = _x;
      let pos = _pos;
      if (!x) {
        return buf;
      }
      let hd = x.hd;
      let len = hd.length;
      Caml_external_polyfill.resolve("blit_string")(hd, 0, buf, pos - len | 0, len);
      _x = x.tl;
      _pos = pos - len | 0;
      continue;
    };
  };
  let _accu = /* [] */0;
  let _len = 0;
  while(true) {
    let len = _len;
    let accu = _accu;
    let n = Caml_external_polyfill.resolve("ml_input_scan_line")(chan);
    if (n === 0) {
      if (accu) {
        return build_result(Caml_bytes.create(len), len, accu);
      }
      throw new Error("End_of_file", {
            cause: {
              RE_EXN_ID: "End_of_file"
            }
          });
    }
    if (n > 0) {
      let res = Caml_bytes.create(n - 1 | 0);
      Caml_external_polyfill.resolve("ml_input")(chan, res, 0, n - 1 | 0);
      Caml_external_polyfill.resolve("ml_input_char")(chan);
      if (!accu) {
        return res;
      }
      let len$1 = (len + n | 0) - 1 | 0;
      return build_result(Caml_bytes.create(len$1), len$1, {
        hd: res,
        tl: accu
      });
    }
    let beg = Caml_bytes.create(-n | 0);
    Caml_external_polyfill.resolve("ml_input")(chan, beg, 0, -n | 0);
    _len = len - n | 0;
    _accu = {
      hd: beg,
      tl: accu
    };
    continue;
  };
}

function close_in_noerr(ic) {
  try {
    return Caml_external_polyfill.resolve("ml_close_channel")(ic);
  }
  catch (exn){
    return;
  }
}

let LargeFile = {};

let exit_function = {
  contents: flush_all
};

function at_exit(f) {
  let g = exit_function.contents;
  exit_function.contents = (function () {
    f();
    g();
  });
}

function do_at_exit() {
  exit_function.contents();
}

function exit(retcode) {
  exit_function.contents();
  return Caml_sys.sys_exit(retcode);
}

Caml_external_polyfill.resolve("register_named_value")("Pervasives.do_at_exit", do_at_exit);

let max_int = 2147483647;

exports.failwith = failwith;
exports.invalid_arg = invalid_arg;
exports.Exit = Exit;
exports.min = min;
exports.max = max;
exports.abs = abs;
exports.lnot = lnot;
exports.max_int = max_int;
exports.min_int = min_int;
exports.infinity = infinity;
exports.neg_infinity = neg_infinity;
exports.nan = nan;
exports.max_float = max_float;
exports.min_float = min_float;
exports.epsilon_float = epsilon_float;
exports.$caret = $caret;
exports.char_of_int = char_of_int;
exports.string_of_bool = string_of_bool;
exports.bool_of_string = bool_of_string;
exports.string_of_int = string_of_int;
exports.valid_float_lexem = valid_float_lexem;
exports.string_of_float = string_of_float;
exports.$at = $at;
exports.open_out_gen = open_out_gen;
exports.open_out = open_out;
exports.open_out_bin = open_out_bin;
exports.flush_all = flush_all;
exports.output_bytes = output_bytes;
exports.output_string = output_string;
exports.output = output;
exports.output_substring = output_substring;
exports.output_value = output_value;
exports.close_out = close_out;
exports.close_out_noerr = close_out_noerr;
exports.open_in_gen = open_in_gen;
exports.open_in = open_in;
exports.open_in_bin = open_in_bin;
exports.input = input;
exports.unsafe_really_input = unsafe_really_input;
exports.really_input = really_input;
exports.really_input_string = really_input_string;
exports.input_line = input_line;
exports.close_in_noerr = close_in_noerr;
exports.LargeFile = LargeFile;
exports.exit_function = exit_function;
exports.at_exit = at_exit;
exports.do_at_exit = do_at_exit;
exports.exit = exit;
/*  Not a pure module */
