const expect = require('chai').expect;

const myParseInt = require('../src/parse-int.js');

describe('my parse int', function () {
  it('turns a string into a number', function () {
    let expected = myParseInt('10');

    expect(expected).to.equal(10);
  });

  it('turns a string starting with 0 into an octal number', function () {
    let expected = myParseInt('010');

    expect(expected).to.equal(8);
  });
});
