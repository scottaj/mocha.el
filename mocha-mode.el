;;; mocha-mode.el --- Mocha testing for Emacs:

;; Copyright (C) 2016 Al Scott <github.com/scottaj>
;; Author: Al Scott
;; URL: http://github.com/pezra/scottaj/mocha-mode
;; Created: 2016
;; Version: 0.1
;; Keywords: javascript mocha jasmine
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; This mode provides the ability to run Mocha or Jasmine tests from within Emacs

;;; Code:
(defvar mocha-which-node "node")

(defvar mocha-command "mocha")

(defvar mocha-environment-variables nil)

(defvar mocha-options nil)

(defvar mocha-project-test-directory nil)

(defun mocha-command (&optional mocha-file)
  "The test command to run.

If MOCHA-FILE is specified run just that file otherwise run MOCHA-PROJECT-TEST-DIRECTORY"
  (concat mocha-environment-variables " "
          mocha-which-node " "
          mocha-command " "
          mocha-options " "
          (or mocha-file mocha-project-test-directory)))

(defun mocha-test-project ()
  "Test the current project."
  (interactive)
  (compile (mocha-command)))

(defun mocha-test-file ()
  "Test the current file."
  (interactive)
  (compile (mocha-command (buffer-file-name))))

(defun mocha-test-at-point ()
  "Test the current innermost 'it' or 'describe' or the file if none is found."
  (interactive)
  (mocha-test-file))

(provide 'mocha-mode)
;;; mocha-mode.el ends here
