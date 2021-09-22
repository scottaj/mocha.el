"use strict";

const {expect} = require("chai");

const myParseFloat = require("../src/parse-float.js");

describe("myParseFloat()", () => {
	it("turns a string into a number", () => {
		expect(myParseFloat("10.5")).to.equal(10.5);
	});
});
