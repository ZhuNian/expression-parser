// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Caml_format = require("bs-platform/lib/js/caml_format.js");

function evaluate(expr) {
  switch (expr.tag | 0) {
    case 0 : 
        return Caml_format.caml_int_of_string(expr[0]);
    case 1 : 
        return evaluate(expr[0]) + evaluate(expr[1]) | 0;
    case 2 : 
        return evaluate(expr[0]) - evaluate(expr[1]) | 0;
    
  }
}

function print(expr) {
  var res = "";
  switch (expr.tag | 0) {
    case 0 : 
        return res + expr[0];
    case 1 : 
        return res + ("Add(" + (print(expr[0]) + ("," + (print(expr[1]) + ")"))));
    case 2 : 
        return res + ("Minus(" + (print(expr[0]) + ("," + (print(expr[1]) + ")"))));
    
  }
}

exports.evaluate = evaluate;
exports.print = print;
/* No side effect */
