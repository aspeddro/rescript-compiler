// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as $$Location from "./location.js";
import * as Longident from "./longident.js";
import * as Ast_helper from "./ast_helper.js";

function decl(attrs, loc, name, alias, typ) {
  return {
          jld_attributes: attrs,
          jld_name: name,
          jld_alias: alias,
          jld_type: typ,
          jld_loc: loc
        };
}

function importDescr(attrs, scope, importSpec, loc) {
  return {
          jid_loc: loc,
          jid_spec: importSpec,
          jid_scope: scope,
          jid_attributes: attrs
        };
}

function toParsetree(importDescr) {
  var bsVal_0 = $$Location.mknoloc("val");
  var bsVal_1 = {
    TAG: /* PStr */0,
    _0: /* [] */0
  };
  var bsVal = [
    bsVal_0,
    bsVal_1
  ];
  var s = importDescr.jid_scope;
  var attrs;
  if (typeof s === "number") {
    attrs = {
      hd: bsVal,
      tl: /* [] */0
    };
  } else if (s.TAG === /* Module */0) {
    var arg = Ast_helper.Str.$$eval;
    var arg$1 = Ast_helper.Exp.constant;
    var structure_0 = Curry._3(arg, undefined, undefined, Curry._3(arg$1, undefined, undefined, {
              TAG: /* Pconst_string */2,
              _0: s._0,
              _1: undefined
            }));
    var structure = {
      hd: structure_0,
      tl: /* [] */0
    };
    var genType_0 = $$Location.mknoloc("genType.import");
    var genType_1 = {
      TAG: /* PStr */0,
      _0: structure
    };
    var genType = [
      genType_0,
      genType_1
    ];
    attrs = {
      hd: genType,
      tl: /* [] */0
    };
  } else {
    var match = List.map((function (s) {
            return Ast_helper.Exp.constant(undefined, undefined, {
                        TAG: /* Pconst_string */2,
                        _0: s,
                        _1: undefined
                      });
          }), Longident.flatten(s._0));
    var expr;
    var exit = 0;
    var exprs;
    if (match && !match.tl) {
      expr = match.hd;
    } else {
      exprs = match;
      exit = 1;
    }
    if (exit === 1) {
      var arg$2 = Ast_helper.Exp.tuple;
      expr = Curry._3(arg$2, undefined, undefined, exprs);
    }
    var structureItem = Ast_helper.Str.$$eval(undefined, undefined, expr);
    var bsScope_0 = $$Location.mknoloc("scope");
    var bsScope_1 = {
      TAG: /* PStr */0,
      _0: {
        hd: structureItem,
        tl: /* [] */0
      }
    };
    var bsScope = [
      bsScope_0,
      bsScope_1
    ];
    attrs = {
      hd: bsVal,
      tl: {
        hd: bsScope,
        tl: /* [] */0
      }
    };
  }
  var decl = importDescr.jid_spec;
  var valueDescrs;
  if (decl.TAG === /* Default */0) {
    var decl$1 = decl._0;
    var prim_0 = decl$1.jld_name;
    var prim = {
      hd: prim_0,
      tl: /* [] */0
    };
    var allAttrs = List.map((function (attr) {
            var id = attr[0];
            if (id.txt !== "genType.import") {
              return attr;
            }
            var match = attr[1];
            if (match.TAG !== /* PStr */0) {
              return attr;
            }
            var match$1 = match._0;
            if (!match$1) {
              return attr;
            }
            var match$2 = match$1.hd.pstr_desc;
            if (match$2.TAG !== /* Pstr_eval */0) {
              return attr;
            }
            if (match$1.tl) {
              return attr;
            }
            var arg = Ast_helper.Exp.constant;
            var $$default = Curry._3(arg, undefined, undefined, {
                  TAG: /* Pconst_string */2,
                  _0: "default",
                  _1: undefined
                });
            var arg$1 = Ast_helper.Str.$$eval;
            var arg$2 = Ast_helper.Exp.tuple;
            var structureItem = Curry._3(arg$1, undefined, undefined, Curry._3(arg$2, undefined, undefined, {
                      hd: match$2._0,
                      tl: {
                        hd: $$default,
                        tl: /* [] */0
                      }
                    }));
            return [
                    id,
                    {
                      TAG: /* PStr */0,
                      _0: {
                        hd: structureItem,
                        tl: /* [] */0
                      }
                    }
                  ];
          }), List.concat({
              hd: attrs,
              tl: {
                hd: importDescr.jid_attributes,
                tl: /* [] */0
              }
            }));
    var arg$3 = Ast_helper.Str.primitive;
    valueDescrs = {
      hd: Curry._2(arg$3, undefined, Ast_helper.Val.mk(importDescr.jid_loc, allAttrs, undefined, prim, $$Location.mknoloc(decl$1.jld_alias), decl$1.jld_type)),
      tl: /* [] */0
    };
  } else {
    valueDescrs = List.map((function (decl) {
            var prim_0 = decl.jld_name;
            var prim = {
              hd: prim_0,
              tl: /* [] */0
            };
            var allAttrs = List.concat({
                  hd: attrs,
                  tl: {
                    hd: decl.jld_attributes,
                    tl: /* [] */0
                  }
                });
            return Ast_helper.Str.primitive(decl.jld_loc, Ast_helper.Val.mk(importDescr.jid_loc, allAttrs, undefined, prim, $$Location.mknoloc(decl.jld_alias), decl.jld_type));
          }), decl._0);
  }
  var jsFfiAttr_0 = $$Location.mknoloc("ns.jsFfi");
  var jsFfiAttr_1 = {
    TAG: /* PStr */0,
    _0: /* [] */0
  };
  var jsFfiAttr = [
    jsFfiAttr_0,
    jsFfiAttr_1
  ];
  var partial_arg = {
    hd: jsFfiAttr,
    tl: /* [] */0
  };
  var partial_arg$1 = importDescr.jid_loc;
  var arg$4 = function (param, param$1) {
    return Ast_helper.Incl.mk(partial_arg$1, partial_arg, param, param$1);
  };
  return Ast_helper.Str.include_(importDescr.jid_loc, Curry._2(arg$4, undefined, Ast_helper.Mod.structure(importDescr.jid_loc, undefined, valueDescrs)));
}

export {
  decl ,
  importDescr ,
  toParsetree ,
  
}
/* Location Not a pure module */
