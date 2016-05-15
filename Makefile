CASK ?= cask
NPM ?= npm

all: install test

test: unit ecukes

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes

install:
	${CASK} install
	cd sample-project && ${NPM} install

.PHONY:	all test unit ecukes install
