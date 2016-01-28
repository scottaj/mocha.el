;;; mocha.el --- Run Mocha or Jasmine tests

;; Copyright (C) 2016 Al Scott <github.com/scottaj>
;; Author: Al Scott
;; URL: http://github.com/scottaj/mocha.el
;; Created: 2016
;; Version: 0.2
;; Keywords: javascript mocha jasmine
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; This mode provides the ability to run Mocha or Jasmine tests from within Emacs

;;; Code:
(require 'compile)

(defgroup mocha nil
  "Tools for running mocha tests."
  :group 'languages)

(defcustom mocha-which-node "node"
  "The path to the node executable to run."
  :type 'string
  :group 'mocha)

(defcustom mocha-command "mocha"
  "The path to the mocha command to run."
  :type 'string
  :group 'mocha)

(defcustom mocha-environment-variables nil
  "Environment variables to run mocha with."
  :type 'string
  :group 'mocha)

(defcustom mocha-options nil
  "Command line options to pass to mocha."
  :type 'string
  :group 'mocha)

(defvar mocha-project-test-directory nil)

(defun mocha-generate-command (&optional mocha-file)
  "The test command to run.

If MOCHA-FILE is specified run just that file otherwise run MOCHA-PROJECT-TEST-DIRECTORY"
  (concat mocha-environment-variables " "
          mocha-which-node " "
          mocha-command " "
          mocha-options " "
          (or mocha-file mocha-project-test-directory)))

(defun mocha-run (&optional mocha-file)
  "Run mocha in a compilation buffer.

If MOCHA-FILE is specified run just that file otherwise run MOCHA-PROJECT-TEST-DIRECTORY"
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))

  (when (get-buffer "*mocha tests*")
    (kill-buffer "*mocha tests*"))
  (let ((test-command-to-run (mocha-generate-command mocha-file)))
    (with-current-buffer (get-buffer-create "*mocha tests*")
      (compilation-start test-command-to-run 'mocha-compilation-mode (lambda (m) (buffer-name))))))

(defvar node-error-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")

(defvar node-error-regexp-alist
  `((,node-error-regexp 1 2 3)))

(defun mocha-compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode mocha-compilation-mode "Mocha"
  "Mocha compilation mode."
  (progn
    (set (make-local-variable 'compilation-error-regexp-alist) node-error-regexp-alist)
    (add-hook 'compilation-filter-hook 'mocha-compilation-filter nil t)
  ))

;;;###autoload
(defun mocha-test-project ()
  "Test the current project."
  (interactive)
  (mocha-run))

;;;###autoload
(defun mocha-test-file ()
  "Test the current file."
  (interactive)
  (mocha-run (buffer-file-name)))

(provide 'mocha)
;;; mocha.el ends here
