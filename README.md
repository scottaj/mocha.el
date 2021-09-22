# `mocha.el`
[![MELPA Status](https://melpa.org/packages/mocha-badge.svg)](https://melpa.org/#/mocha)
[![MELPA Stable Status](https://stable.melpa.org/packages/mocha-badge.svg)](https://stable.melpa.org/#/mocha)
[![Build Status](https://travis-ci.org/scottaj/mocha.el.svg?branch=master)](https://travis-ci.org/scottaj/mocha.el)

Emacs helpers for running [Mocha](https://mochajs.org/) tests. It lets you test an entire project, a particular file, or a particular test in that file.

![Mocha.el in Action](https://raw.githubusercontent.com/scottaj/mocha.el/master/mocha.png)

## Contents
 * [Installation](#installation)
 * [Configuration](#configuration)
 * [Running tests](#running-tests)
 * [Running test at point](#running-test-at-point)
 * [Debugging tests](#debugging-tests)

## Installation
The package can be installed via MELPA. The package name is [`mocha`](https://melpa.org/#/mocha).

## Configuration
Everything is set up right now to be configured via customize or per project in a `.dir-locals.el` file. You can find all options by looking at the `mocha` group in the customize interface. Below is an example configuration:

```elisp
((nil . (
    (mocha-which-node . "/Users/ajs/.nvm/versions/node/v4.2.2/bin/node")
    (mocha-command . "node_modules/.bin/mocha")
    (mocha-environment-variables . "NODE_ENV=test")
    (mocha-options . "--recursive --reporter dot -t 5000")
    (mocha-project-test-directory . "test")
    (mocha-reporter . "spec")
)))
```

## Running tests
In order to run tests there are three functions exposed: 

1. `mocha-test-project` will run all the tests in your project. 
2. `mocha-test-file` will test just the current file you are visiting.
3. `mocha-test-at-point` will try and semantically find the nearest enclosing `it` or `describe` from your cursor, and just run that.

You can run any of these functions via <kbd>M-x</kbd>, or assign them to hotkeys.

Stack traces for failing tests have clickable links to the file and line that failed.

## Running test at point

`mocha-test-at-point` uses [`js2-mode`](https://github.com/mooz/js2-mode) to find the nearest `describe` or `it` and extract the description string from it. As such, it only works in JavaScript files that have `js2-mode` set as the major-mode.

Mocha includes an `imenu` function that builds an index matching the `describe` and `it` tree. You can enable this index function by setting `imenu-create-index-function` to `mocha-make-imenu-alist` or using `(mocha-toggle-imenu-function)`.

## Debugging tests

Each of the test functions has a debug analog: `mocha-debug-project`, `mocha-debug-file`, and `mocha-debug-at-point`. Using these functions depends on having a JavaScript debugger installed and loaded. The debuggers with built-in support are:

* [`realgud`](https://github.com/realgud/realgud) (via [`realgud-node-inspect`](http://github.com/realgud/realgud-node-inspect))
* [`indium`](https://indium.readthedocs.io/en/latest/debugger.html)

The `realgud` debugging buffer takes the same commands as the standard node CLI debugger. Some useful ones are:
* <kbd>s</kbd> to step in
* <kbd>o</kbd> to step out
* <kbd>n</kbd> to step over
* <kbd>c</kbd> to continue execution
* <kbd>repl</kbd> use interactive REPL at point
* <kbd>C-c C-c</kbd> to send a BREAK signal
* <kbd>M-p</kbd> to cycle through previous inputs

## Contribution

Be sure to!

Install [Cask](https://github.com/cask/cask) if you haven't already.

Install the dependencies:

~~~console
$ make install
~~~

Run the tests with:

~~~console
$ make test
~~~
