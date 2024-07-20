// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Lexing = require("../../lib/js/lexing.js");
let Pervasives = require("../../lib/js/pervasives.js");
let Caml_format = require("../../lib/js/caml_format.js");

let __ocaml_lex_tables = {
  lex_base: "\x00\x00\xf6\xff\xf7\xff\xf8\xff\xf9\xff\xfa\xff\xfb\xff\xfc\xff\
    \x3a\x00\x85\x00\xff\xff",
  lex_backtrk: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \x02\x00\x01\x00\xff\xff",
  lex_default: "\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \xff\xff\xff\xff\x00\x00",
  lex_trans: "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x0a\x00\x0a\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x03\x00\x02\x00\x05\x00\x07\x00\x00\x00\x06\x00\x00\x00\x04\x00\
    \x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\
    \x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x09\x00\x09\x00\x09\x00\
    \x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
  lex_check: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\x00\x00\x00\x00\xff\xff\xff\xff\x00\x00\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
    \x00\x00\x00\x00\x00\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\
    \x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x09\x00\x09\x00\x09\x00\
    \x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
    \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
  lex_base_code: "",
  lex_backtrk_code: "",
  lex_default_code: "",
  lex_trans_code: "",
  lex_check_code: "",
  lex_code: ""
};

function __ocaml_lex_lexeme_rec(lexbuf, ___ocaml_lex_state) {
  while(true) {
    let __ocaml_lex_state = ___ocaml_lex_state;
    let __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          ___ocaml_lex_state = 0;
          continue;
      case 1 :
          return {
            TAG: "NUMERAL",
            _0: Caml_format.int_of_string(Lexing.lexeme(lexbuf))
          };
      case 2 :
          return {
            TAG: "IDENT",
            _0: Lexing.lexeme(lexbuf)
          };
      case 3 :
          return "PLUS";
      case 4 :
          return "MINUS";
      case 5 :
          return "TIMES";
      case 6 :
          return "DIVIDE";
      case 7 :
          return "LPAREN";
      case 8 :
          return "RPAREN";
      case 9 :
          return "EOF";
      default:
        lexbuf.refill_buff(lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue;
    }
  };
}

function lexeme(lexbuf) {
  return __ocaml_lex_lexeme_rec(lexbuf, 0);
}

function str(e) {
  switch (e.TAG) {
    case "Numeral" :
        return Pervasives.string_of_float(e._0);
    case "Plus" :
        return str(e._0) + ("+" + str(e._1));
    case "Minus" :
        return str(e._0) + ("-" + str(e._1));
    case "Times" :
        return str(e._0) + ("*" + str(e._1));
    case "Divide" :
        return str(e._0) + ("/" + str(e._1));
    case "Negate" :
        return "-" + str(e._0);
    case "Variable" :
        return e._0;
    
  }
}

exports.__ocaml_lex_tables = __ocaml_lex_tables;
exports.lexeme = lexeme;
exports.__ocaml_lex_lexeme_rec = __ocaml_lex_lexeme_rec;
exports.str = str;
/* No side effect */
