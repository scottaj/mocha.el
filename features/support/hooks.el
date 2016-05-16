(require 'ecukes)

(defun kill-js-buffers ()
  (mapcar (lambda (buffer)
            (when (string-match "\.js" (buffer-name buffer))
              (kill-buffer (buffer-name buffer))))
          (buffer-list))) 

(After ;; Kill all test buffers
 (when (get-buffer "*mocha tests*")
   (kill-buffer "*mocha tests*"))
 (kill-js-buffers))
