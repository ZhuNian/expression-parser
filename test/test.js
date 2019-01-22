var parser = require('../src/parser.bs').parse
var assert = require('assert');
describe('expression parser', function() {
  describe('evaluate', function() {
    // it('should return correct', function() {
    //   assert.equal(parser(""), "failed: ");
    // });

    it('should return correct', function() {
      assert.equal(parser("1"), "1");
    });

    it('should return correct', function() {
      assert.equal(parser("123"), "123");
    });

    it('should return correct', function() {
      assert.equal(parser("ad123e"), "failed: ad123e");
    });
    

    it('should return correct', function() {
      assert.equal(parser("1-2+3-4"), "-2");
    });

    it('should return correct', function() {
      assert.equal(parser("1+2-3+4"), "4");
    });


    it('should return correct', function() {
      assert.equal(parser("1-234+34-4"), "-203");
    });

    it('should return correct', function() {
      assert.equal(parser("12+2-343+45"), "-284");
    });   
    
    it('should return correct', function() {
      assert.equal(parser("-1"), "-1");
    });
    
    it('should return correct', function() {
      assert.equal(parser("-123"), "-123");
    });

    it('should return correct', function() {
      assert.equal(parser("-12+3-4+5"), "-8");
    }); 


    it('should return correct', function() {
      assert.equal(parser("-ad123e"), "failed: -ad123e");
    });


    it('should return correct', function() {
      assert.equal(parser("+123"), "failed: +123");
    });

    // it('should return correct', function() {
    //   assert.equal(parser("123e"), "failed: 123e");
    // });
  });
});