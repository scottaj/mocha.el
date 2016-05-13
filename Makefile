CASK ?= cask
NPM ?= npm

all: install node packages test

test: unit ecukes

unit:
	${CASK} exec ert-runner

ecukes:
	${CASK} exec ecukes

install:
	${CASK} install

node packages:
	cd sample-project && ${NPM} install

.PHONY:	all test unit ecukes install
