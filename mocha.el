;;; Package --- Summary:

;;; Summary:

;;; Commentary:

;;; Code:
(defvar mocha-command "mocha")

(defvar mocha-environment-variables nil)

(defvar mocha-reporter "dot")

(defvar mocha-options nil)

(defvar mocha-project-test-directory nil)

(defun mocha-test-project ()
  "Test the current project."
  nil)

(defun mocha-test-file ()
  "Test the current file."
  nil)

(defun mocha-test-at-point ()
  "Test the current innermost 'it' or 'describe' or the file if none is found."
  nil)

(provide 'mocha)
;;; mocha.el ends here
