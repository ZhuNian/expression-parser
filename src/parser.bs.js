// Generated by BUCKLESCRIPT VERSION 4.0.14, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Js_exn = require("bs-platform/lib/js/js_exn.js");
var Evaluate$ExpressionParserDemo = require("./evaluate.bs.js");

function digitalParser(input) {
  var c = input.charAt(0);
  if ("0123456789".includes(c)) {
    return /* Ok */Block.__(0, [
              c,
              input.slice(1, input.length)
            ]);
  } else {
    return /* Error */Block.__(1, ["not digital"]);
  }
}

function operatorParser(input) {
  var c = input.charAt(0);
  if (c === "+" || c === "-") {
    return /* Ok */Block.__(0, [
              c,
              input.slice(1, input.length)
            ]);
  } else {
    return /* Error */Block.__(1, ["not known operator"]);
  }
}

function validInputParser(input) {
  if (input === "") {
    return /* Error */Block.__(1, ["empty string"]);
  } else {
    return /* Ok */Block.__(0, [
              input,
              ""
            ]);
  }
}

function $at$great(p1, p2, input) {
  var match = Curry._1(p1, input);
  if (match.tag) {
    return /* Error */Block.__(1, [match[0]]);
  } else {
    return Curry._1(p2, match[0]);
  }
}

function $at$pipe(p1, p2, input) {
  var match = Curry._1(p1, input);
  if (match.tag) {
    return /* Ok */Block.__(0, [
              input,
              ""
            ]);
  } else {
    return Curry._1(p2, match[0]);
  }
}

function many(p, input) {
  var match = $at$great(validInputParser, p, input);
  if (match.tag) {
    return /* Error */Block.__(1, [match[0]]);
  } else {
    var remaining = match[1];
    var current = match[0];
    var match$1 = many(p, remaining);
    if (match$1.tag) {
      return /* Ok */Block.__(0, [
                current,
                remaining
              ]);
    } else {
      return /* Ok */Block.__(0, [
                current + match$1[0],
                match$1[1]
              ]);
    }
  }
}

function parser(_input, _expr, _param) {
  while(true) {
    var expr = _expr;
    var input = _input;
    console.log(input);
    var match = many(digitalParser, input);
    if (match.tag) {
      return Js_exn.raiseError(match[0]);
    } else {
      var current = match[0];
      var match$1 = $at$pipe(validInputParser, operatorParser, match[1]);
      if (match$1.tag) {
        return Js_exn.raiseError(match$1[0]);
      } else {
        var remaining1 = match$1[1];
        var param;
        if (expr !== undefined) {
          param = /* tuple */[
            expr,
            remaining1
          ];
        } else {
          var match$2 = many(digitalParser, remaining1);
          param = match$2.tag ? Js_exn.raiseError("next expr error") : /* tuple */[
              /* Digital */Block.__(0, [match$2[0]]),
              match$2[1]
            ];
        }
        var remaining_ = param[1];
        var expr_ = param[0];
        switch (match$1[0]) {
          case "" : 
              return expr_;
          case "+" : 
              _param = /* () */0;
              _expr = /* Add */Block.__(1, [
                  expr_,
                  /* Digital */Block.__(0, [current])
                ]);
              _input = remaining_;
              continue ;
          case "-" : 
              _param = /* () */0;
              _expr = /* Minus */Block.__(2, [
                  expr_,
                  /* Digital */Block.__(0, [current])
                ]);
              _input = remaining_;
              continue ;
          default:
            return Js_exn.raiseError("parse error");
        }
      }
    }
  };
}

console.log(Evaluate$ExpressionParserDemo.print(parser("1+2-3+4", undefined, /* () */0)));

exports.digitalParser = digitalParser;
exports.operatorParser = operatorParser;
exports.validInputParser = validInputParser;
exports.$at$great = $at$great;
exports.$at$pipe = $at$pipe;
exports.many = many;
exports.parser = parser;
/*  Not a pure module */
