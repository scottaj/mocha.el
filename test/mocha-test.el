
;;;; mocha-find-project-root

(ert-deftest mocha-test/mocha-find-project-root/return-path-to-project-root ()
  (mocha-test/with-sandbox
   (should (f-same? (mocha-find-project-root) mocha-test/sandbox-path))))

(ert-deftest mocha-test/mocha-find-project-root/return-nil-unless-package-file-exist ()
  (mocha-test/with-sandbox
   (f-delete "package.json" :force)
   (should-not (mocha-find-project-root))))
