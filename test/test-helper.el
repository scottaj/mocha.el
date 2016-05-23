(require 'f)
(require 's)

(defvar mocha-test/test-path (f-parent (f-this-file)))
(defvar mocha-test/root-path (f-parent mocha-test/test-path))
(defvar mocha-test/sandbox-path (f-join mocha-test/test-path "sandbox"))

(require 'mocha (f-join mocha-test/root-path "mocha.el"))

(defmacro mocha-test/with-sandbox (&rest body)
  `(progn
     (when (f-dir? mocha-test/sandbox-path)
       (f-delete mocha-test/sandbox-path :force))
     (f-mkdir mocha-test/sandbox-path)
     (f-touch (f-join mocha-test/sandbox-path "package.json"))
     (let ((default-directory (f-slash mocha-test/sandbox-path)))
       (f-with-sandbox default-directory ,@body))))
