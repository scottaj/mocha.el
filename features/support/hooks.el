(require 'ecukes)
(require 'f)

(defun kill-js-buffers ()
  (mapc (lambda (buffer)
            (when (f-ext? (or (buffer-file-name buffer) "") "js")
              (kill-buffer (buffer-name buffer))))
          (buffer-list))) 

(After ;; Kill all test buffers
 (when (get-buffer "*mocha tests*")
   (kill-buffer "*mocha tests*"))
 (kill-js-buffers))
