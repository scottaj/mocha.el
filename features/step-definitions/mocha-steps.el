(require 'ert)
(require 'f)

(defvar ecukes--waiting-for-compilation nil)

(When "^I visit sample project file \"\\([^\"]+\\)\"$"
      (lambda (file)
        (find-file (f-expand file "sample-project"))))

(When "^I run the command \"\\([^\"]+\\)\"$"
      (lambda (command)
        (When "I start an action chain")
        (And "I press \"M-x\"")
        (And (format "I type %S" command))
        (And "I execute the action chain")
        (And "I wait for the compilation to finish")))

(Then "^I should see buffer \"\\([^\"]+\\)\"$"
      (lambda (buffer-name)
        (let ((buffer-names (-map 'buffer-name (-map 'window-buffer (window-list)))))
          (should (-contains? buffer-names buffer-name)))))

(Then "^I should see contents in buffer \"\\([^\"]+\\)\":$"
      (lambda (buffer contents)
        (with-current-buffer buffer
          (should (s-contains? contents (buffer-substring-no-properties (point-min) (point-max)))))))

(When "^I wait for the compilation to finish$"
      (lambda ()
        (setq ecukes--waiting-for-compilation t)

        (defun ecukes--compilation-finished (&rest ignore)
          (setq ecukes--waiting-for-compilation nil)
          (remove-hook 'compilation-finish-functions 'ecukes--compilation-finished))

        (add-hook 'compilation-finish-functions 'ecukes--compilation-finished)

        (while ecukes--waiting-for-compilation
          (accept-process-output nil 0.005))))
