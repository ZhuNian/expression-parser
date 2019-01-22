// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Utils$ExpressionParserDemo = require("./utils.bs.js");
var Evaluate$ExpressionParserDemo = require("./evaluate.bs.js");
var Combinators$ExpressionParserDemo = require("./combinators.bs.js");

function pChar(charToMatch) {
  var innerFn = function (input) {
    var firstChar = input.charAt(0);
    if (charToMatch === firstChar) {
      return /* Ok */Block.__(0, [/* tuple */[
                  firstChar,
                  input.slice(1, input.length)
                ]]);
    } else {
      return /* Error */Block.__(1, ["expected: " + (charToMatch + ("; but got: " + input))]);
    }
  };
  return /* Parser */[innerFn];
}

var addCharParser = pChar("+");

var minusCharParser = pChar("-");

var emptyParser = pChar("");

var digitalCharParser = Combinators$ExpressionParserDemo.anyOf(/* array */[
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9"
      ].map(pChar));

function mapFn(s1, s2) {
  return s1 + s2;
}

function addThen_(p1, p2) {
  return Combinators$ExpressionParserDemo.lift2(mapFn, p1, p2);
}

function many(parser) {
  var innerFn = function (input) {
    return Utils$ExpressionParserDemo.$unknown$bang$eq(Combinators$ExpressionParserDemo.run(many(parser), input), (function (param) {
                  return /* Ok */Block.__(0, [/* tuple */[
                              "",
                              input
                            ]]);
                }));
  };
  return Combinators$ExpressionParserDemo.$unknown$pipe(Combinators$ExpressionParserDemo.lift2(mapFn, parser, /* Parser */[innerFn]), emptyParser);
}

var positiveDigitalParser = Combinators$ExpressionParserDemo.$unknown$eq(many(digitalCharParser), (function (res) {
        return /* Digital */Block.__(0, [res]);
      }));

var digitalParser = Combinators$ExpressionParserDemo.$unknown$pipe(Combinators$ExpressionParserDemo.$unknown$eq(Combinators$ExpressionParserDemo.$unknown$great(minusCharParser, positiveDigitalParser), (function (param) {
            return /* Minus */Block.__(2, [
                      /* Digital */Block.__(0, ["0"]),
                      param[1]
                    ]);
          })), positiveDigitalParser);

var addParser = Combinators$ExpressionParserDemo.$unknown$eq(Combinators$ExpressionParserDemo.$unknown$great(Combinators$ExpressionParserDemo.$unknown$great(digitalParser, addCharParser), positiveDigitalParser), (function (param) {
        return /* Add */Block.__(1, [
                  param[0][0],
                  param[1]
                ]);
      }));

var minusParser = Combinators$ExpressionParserDemo.$unknown$eq(Combinators$ExpressionParserDemo.$unknown$great(Combinators$ExpressionParserDemo.$unknown$great(digitalParser, minusCharParser), positiveDigitalParser), (function (param) {
        return /* Minus */Block.__(2, [
                  param[0][0],
                  param[1]
                ]);
      }));

var expressionParser = Combinators$ExpressionParserDemo.$unknown$pipe(Combinators$ExpressionParserDemo.$unknown$pipe(addParser, minusParser), digitalParser);

function parse(_input) {
  while(true) {
    var input = _input;
    var match = Combinators$ExpressionParserDemo.run(expressionParser, input);
    if (match.tag) {
      return "failed: " + match[0];
    } else {
      var match$1 = match[0];
      var remain = match$1[1];
      var exp = match$1[0];
      if (remain === "") {
        return String(Evaluate$ExpressionParserDemo.evaluate(exp));
      } else {
        _input = String(Evaluate$ExpressionParserDemo.evaluate(exp)) + remain;
        continue ;
      }
    }
  };
}

function logParse(input) {
  console.log(parse(input));
  return /* () */0;
}

var $unknown$unknown$eq = addThen_;

exports.pChar = pChar;
exports.addCharParser = addCharParser;
exports.minusCharParser = minusCharParser;
exports.emptyParser = emptyParser;
exports.digitalCharParser = digitalCharParser;
exports.mapFn = mapFn;
exports.addThen_ = addThen_;
exports.$unknown$unknown$eq = $unknown$unknown$eq;
exports.many = many;
exports.positiveDigitalParser = positiveDigitalParser;
exports.digitalParser = digitalParser;
exports.addParser = addParser;
exports.minusParser = minusParser;
exports.expressionParser = expressionParser;
exports.parse = parse;
exports.logParse = logParse;
/* addCharParser Not a pure module */
