
;;;; mocha-opts-file

(ert-deftest mocha-test/mocha-opts-file/return-correct-opts-file ()
  (mocha-test/with-sandbox
   (f-mkdir (f-join default-directory "test"))
   (f-mkdir (f-join default-directory "test" "unit"))
   (f-mkdir (f-join default-directory "test" "acceptance"))
   (f-mkdir (f-join default-directory "test" "integration"))
   (let ((unit-test-file (f-join default-directory "test" "unit" "test.coffee"))
         (unit-opts-file (f-join default-directory "test" "unit" "mocha.opts"))
         (acceptance-test-dir (f-join default-directory "test" "acceptance"))
         (acceptance-opts-file (f-join default-directory "test" "acceptance" "mocha.opts"))
         (integration-test-file (f-join default-directory "test" "integration" "test.coffee")))
     (f-touch unit-test-file)
     (f-touch unit-opts-file)
     (f-touch acceptance-opts-file)
     (f-touch integration-test-file)
     (should (equal (mocha-opts-file unit-test-file) unit-opts-file))
     (should (equal (mocha-opts-file acceptance-test-dir) acceptance-opts-file))
     (should-not (mocha-opts-file integration-test-file)))))


;;;; mocha-generate-command

(ert-deftest mocha-test/mocha-generate-command/return-command-including-mocha-opts-option ()
  (mocha-test/with-sandbox
   (f-mkdir (f-join default-directory "test"))
   (f-mkdir (f-join default-directory "test" "unit"))
   (f-mkdir (f-join default-directory "test" "integration"))
   (let ((unit-test-file (f-join default-directory "test" "unit" "test.coffee"))
         (unit-opts-file (f-join default-directory "test" "unit" "mocha.opts"))
         (integration-test-file (f-join default-directory "test" "integration" "test.coffee")))
     (f-touch unit-test-file)
     (f-touch unit-opts-file)
     (f-touch integration-test-file)
     (should (s-contains? (concat "--opts " unit-opts-file) (mocha-generate-command nil unit-test-file)))
     (should-not (s-contains? "--opts" (mocha-generate-command nil integration-test-file))))))

(ert-deftest mocha-test/mocha-generate-command/return-command-with-correct-reporter ()
  (mocha-test/with-sandbox
   (should (s-contains? "--reporter dot" (mocha-generate-command nil)))
   (let ((mocha-reporter "spec"))
     (should (s-contains? "--reporter spec" (mocha-generate-command nil))))))


;;;; mocha-find-project-root

(ert-deftest mocha-test/mocha-find-project-root/return-path-to-project-root ()
  (mocha-test/with-sandbox
   (should (f-same? (mocha-find-project-root) mocha-test/sandbox-path))
   (should (s-ends-with? "/" (mocha-find-project-root)))
   (let ((foo-dir (f-join default-directory "foo")))
     (f-mkdir foo-dir)
     (let ((default-directory foo-dir))
       (should (f-same? (mocha-find-project-root) mocha-test/sandbox-path))))))

(ert-deftest mocha-test/mocha-find-project-root/return-nil-unless-package-file-exist ()
  (mocha-test/with-sandbox
   (f-delete "package.json" :force)
   (should-not (mocha-find-project-root))))


;;;; node-error-regexp

(ert-deftest mocha-test/node-error-regexp/unix-path ()
  (let ((line "    at Connection.parseE (/app/pitia-server/node_modules/pg/lib/connection.js:554:11)"))
    (should (string-match node-error-regexp line))
    ;; 1 is file, 2 is line, 3 is column
    (should (string= (match-string (nth 1 (car node-error-regexp-alist)) line)
                     "/app/pitia-server/node_modules/pg/lib/connection.js"))
    (should (string= (match-string (nth 2 (car node-error-regexp-alist)) line) "554"))
    (should (string= (match-string (nth 3 (car node-error-regexp-alist)) line) "11"))))

(ert-deftest mocha-test/node-error-regexp/windows-path ()
  (let ((line
         "    at Timeout.callback [as _onTimeout] (node_modules\\jsdom\\lib\\jsdom\\browser\\Window.js:477:19)"))
    (should (string-match node-error-regexp line))
    ;; 1 is file, 2 is line, 3 is column
    (should (string= (match-string (nth 1 (car node-error-regexp-alist)) line)
                     "node_modules\\jsdom\\lib\\jsdom\\browser\\Window.js"))
    (should (string= (match-string (nth 2 (car node-error-regexp-alist)) line) "477"))
    (should (string= (match-string (nth 3 (car node-error-regexp-alist)) line) "19"))))



;;;; mocha-run

(ert-deftest mocha-test/mocha-run/buffer-local-compilation-env ()
  (with-temp-buffer
    (make-local-variable 'compilation-environment)
    (setq compilation-environment '("TEST=abc"))
    (let ((command (if (memq system-type '(windows-nt ms-dos))
                       "echo %TEST%"
                     "echo $TEST"))
          (old-mocha-generate-command (symbol-function 'mocha-generate-command))
          (old-mocha-find-project-root (symbol-function 'mocha-find-project-root))
          (old-cd (symbol-function 'cd)))
      (unwind-protect
          (progn
            (fset 'mocha-generate-command (lambda (debug &optional mocha-file test) command))
            (fset 'mocha-find-project-root (lambda () "."))
            (fset 'cd (lambda (dir) "."))
            (mocha-run))
        (fset 'mocha-generate-command old-mocha-generate-command)
        (fset 'mocha-find-project-root old-mocha-find-project-root)
        (fset 'cd old-cd))
      (sit-for 2)
      (with-current-buffer "*mocha tests*"
        (goto-char 0)
        (should (search-forward (concat "\n" command "\n") nil t))
        (should (looking-at-p "abc"))))))

(ert-deftest mocha-test/node-error-regexp/windows-path-with-drive ()
  (let ((line
         "    at Timeout.callback [as _onTimeout] (C:\\Users\\name\\node_modules\\jsdom\\lib\\jsdom\\browser\\Window.js:477:19)"))
    (should (string-match node-error-regexp line))
    ;; 1 is file, 2 is line, 3 is column
    (should (string= (match-string (nth 1 (car node-error-regexp-alist)) line)
                     "C:\\Users\\name\\node_modules\\jsdom\\lib\\jsdom\\browser\\Window.js"))
    (should (string= (match-string (nth 2 (car node-error-regexp-alist)) line) "477"))
    (should (string= (match-string (nth 3 (car node-error-regexp-alist)) line) "19"))))
