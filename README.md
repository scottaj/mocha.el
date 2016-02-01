[![Melpa Status](http://melpa.milkbox.net/packages/mocha-badge.svg)](http://melpa.milkbox.net/#/mocha)
[![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/mocha-badge.svg)](http://melpa-stable.milkbox.net/#/mocha)

# mocha.el
Emacs helpers for running mocha tests

Right now this is pretty bare-bones. It lets you test an entire project or a particular file.

Everything is set up right now to be configured via customize or in local variables in a `.dir-locals.el`. Below is an example configuration:

```
((nil . (
            (mocha-which-node . "/Users/ajs/.nvm/versions/node/v4.2.2/bin/node")
            (mocha-command . "node_modules/.bin/_mocha")
            (mocha-environment-variables . "NODE_ENV=test")
            (mocha-options . "--recursive --reporter dot -t 5000")
            (mocha-project-test-directory . "test")
            )))

```

In order to run tests there are three functions exposed: 

1. `mocha-test-project` will run all the tests in your project. 
1. `mocha-test-file` will test just the current file you are visiting with mocha.
1. `mocha-test-at-point` will try and semantically find the nearest enclosing `it` or `describe`, and just run that.

You can run any of these functions via `M-x`, or assign them to hotkeys.
