// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");

function bind_(result, f) {
  if (result.tag) {
    return /* Error */Block.__(1, [result[0]]);
  } else {
    return Curry._1(f, result[0]);
  }
}

function or_(result, f) {
  if (result.tag) {
    return Curry._1(f, result[0]);
  } else {
    return /* Ok */Block.__(0, [result[0]]);
  }
}

var $neg$neg$great = bind_;

var $unknown$bang$eq = or_;

exports.bind_ = bind_;
exports.$neg$neg$great = $neg$neg$great;
exports.or_ = or_;
exports.$unknown$bang$eq = $unknown$bang$eq;
/* No side effect */
