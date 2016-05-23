const expect = require('chai').expect;

const myParseFloat = require('../src/parse-float.js');

describe('my parse float', function () {
  it('turns a string into a number', function () {
    let expected = myParseFloat('10.5');

    expect(expected).to.equal(10.5);
  });
});
