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

In order to run tests there are two functions exposed. `mocha-test-project` will run all the tests in your project. `mocha-test-file` will try to test just the current file you are visiting with mocha.

You can run either of these functions via `M-x`, or assign them to hotkeys.
