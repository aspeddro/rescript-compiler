// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function show(x) {
  let a = x._0;
  if (a === 0 && x._1 === 0 && x._2 === 0) {
    return "zeroes";
  }
  return a.toString() + x._1.toString();
}

exports.show = show;
/* No side effect */
