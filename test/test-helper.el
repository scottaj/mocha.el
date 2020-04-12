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

(defmacro mocha-dynamic-flet (bindings &rest body)
  "Set BINDINGS and execute BODY.
BINDINGS is a list of (name arglist body) lists of functions to
dynamically redefine, or simply (name).  In the latter case, name
will be given to `fmakunbound'."
  ;; can't use noflet because it doesn't support 24.x
  (declare (debug ((&rest (&define name &optional lambda-list def-body)) body))
           (indent 1))
  (let ((old-new-pairs
         (mapcar (lambda (b) (cons (make-symbol (format "old-%s" (car b))) b))
                 bindings)))
    `(let ,(mapcar (lambda (old-new)
                     `(,(car old-new) (symbol-function ',(cadr old-new))))
                   old-new-pairs)
       (unwind-protect
           (progn
             ,@(mapcar (lambda (b)
                         (if (cdr b)
                             `(setf (symbol-function ',(car b))
                                    #'(lambda ,(cadr b) ,@(cddr b)))
                           `(fmakunbound ',(car b))))
                       bindings)
             ,@body)
         ,@(mapcar (lambda (old-new)
                     `(setf (symbol-function ',(cadr old-new)) ,(car old-new)))
                   old-new-pairs)))))

;;; test-helper.el ends here
