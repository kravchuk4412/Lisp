(uiop:define-package :hw9/tests/all
    (:use :cl
          :hw9/tests/cfg)
  (:export #:test-suite))

(in-package :hw9/tests/all)

(defun test-suite ()
  (run-cfg-tests))
