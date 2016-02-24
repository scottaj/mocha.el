[![Melpa Status](http://melpa.milkbox.net/packages/mocha-badge.svg)](http://melpa.milkbox.net/#/mocha)
[![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/mocha-badge.svg)](http://melpa-stable.milkbox.net/#/mocha)

# mocha.el
Emacs helpers for running mocha tests. It lets you test an entire project, a particular file, or a particular test in that file.

![Mocha.el in Action](https://raw.githubusercontent.com/scottaj/mocha.el/master/mocha.png)

## Contents
 * [Installation](#installation)
 * [Configuration](#configuration)
 * [Running Tests](#running-tests)
 * [Running Test at Point](#running-test-at-point)
 * [Debugging Tests](#debugging-tests)

## Installation
The package can be installed via MELPA. The package name is mocha.

## Configuration

Everything is set up right now to be configured via customize or per project in a `.dir-locals.el` file. You can find all options by looking at the `mocha` group in the customize interface. Below is an example configuration:

```
((nil . (
            (mocha-which-node . "/Users/ajs/.nvm/versions/node/v4.2.2/bin/node")
            (mocha-command . "node_modules/.bin/mocha")
            (mocha-environment-variables . "NODE_ENV=test")
            (mocha-options . "--recursive --reporter dot -t 5000")
            (mocha-project-test-directory . "test")
            )))
```

I highly recommend using the "dot" reporter with Emacs. The ansi escaping code within emacs generally does a bad job with the other reporters 

## Running tests

In order to run tests there are three functions exposed: 

1. `mocha-test-project` will run all the tests in your project. 
1. `mocha-test-file` will test just the current file you are visiting.
1. `mocha-test-at-point` will try and semantically find the nearest enclosing `it` or `describe` from your cursor, and just run that.

You can run any of these functions via `M-x`, or assign them to hotkeys.

Stack traces for failing tests have clickable links to the file and line that failed.

## Running Test at Point

`mocha-test-at-point` uses [js2-mode](https://github.com/mooz/js2-mode) to find the nearest `describe` or `it` and extract the description string from it. As such, it only works in JavaScript files that have js2-mode set as the major mode.

## Debugging Tests

Each of the test functions has a debug analog: `mocha-debug-project`, `mocha-debug-file`, and `mocha-debug-at-point`. Using these functions depends on having [realgud](https://github.com/rocky/emacs-dbgr) installed and loaded.

The provided debugging buffer takes the same commands as the standard node CLI debugger. Some useful ones are:
 * `s` to step in
 * `o` to step out
 * `n` to step over
 * `c` to continue execution
 * `repl` use interactive REPL at point
 
 Additionally `C-c C-c` will send a BREAK signal, and `M-p` will cycle through previous inputs.
