(require 'ecukes)

(defvar mocha-init-buffers (buffer-list))

(defun mocha-kill-buffers ()
  (mapc
   (lambda (buffer)
     (unless (member buffer mocha-init-buffers)
       (kill-buffer buffer)))
   (buffer-list)))

(Before
 (mocha-kill-buffers))
