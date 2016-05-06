(require 'f)

(defvar mocha-support-path
  (f-dirname load-file-name))

(defvar mocha-features-path
  (f-parent mocha-support-path))

(defvar mocha-root-path
  (f-parent mocha-features-path))

(add-to-list 'load-path mocha-root-path)

(require 'mocha)
(require 'espuds)
(require 'ert)
