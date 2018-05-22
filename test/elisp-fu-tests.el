;;; elisp-fu-tests.el --- Tests for elisp-fu

(require 'elisp-fu)

(defvar elisp-fu--test-var 1)

(ert-deftest elisp-fu--unbind-defvar ()
  "Calling unbind on a defvar form should make the variable unbound."
  (should (boundp 'elisp-fu--test-var))

  (elisp-fu--unbind '(defvar elisp-fu--test-var 1))
  (should (not (boundp 'elisp-fu--test-var)))

  ;; Bind the variable again, so we can run the test repeatedly.
  (setq elisp-fu--test-var 1))

(ert-deftest elisp-fu--unbind-setq ()
  "Calling unbind on a setq form should leave the variable bound."
  (should (boundp 'elisp-fu--test-var))

  (elisp-fu--unbind '(setq elisp-fu--test-var 1))
  (should (boundp 'elisp-fu--test-var)))

;;; elisp-fu-tests.el ends here
