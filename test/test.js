var parser = require('../src/parser.bs')
var assert = require('assert');
describe('expression parser', function() {
  describe('empty string', function() {
    it('should return empty when the value is not present', function() {
      assert.equal(parser(""), "");
    });

    it('should return digital when the value is not present', function() {
      assert.equal(parser("1"), "1");
    });

    it('should return digital when the value is not present', function() {
      assert.equal(parser("123"), "123");
    });

    it('should return error when the value is not present', function() {
      assert.equal(parser("123e"), "parser error");
    });
  });
});