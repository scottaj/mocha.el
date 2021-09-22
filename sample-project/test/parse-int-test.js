"use strict";

const {expect} = require("chai");

const myParseInt = require("../src/parse-int.js");

describe("myParseInt()", () => {
	it("turns a string into an integer", () => {
		const expected = myParseInt("10");
		expect(expected).to.equal(10);
	});

	// NB: This test is expected to fail
	it("turns a string starting with 0 into an octal number", () => {
		expect(myParseInt("010")).to.equal(8);
	});
});
