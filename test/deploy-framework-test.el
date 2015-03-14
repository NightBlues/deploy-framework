;;; deploy-framework-test.el --- deploy-framework test suite

;;; Commentary:

;;; Code:

(require 'ert)
(require 'deploy-framework)

(ert-deftest test-df-make-command ()
  "Test make command generator"
  (should (equal (df-make-command "/tmp" :make-args "no-lint") (cons "/tmp" "make no-lint"))))

;;; deploy-framework-test.el ends here
