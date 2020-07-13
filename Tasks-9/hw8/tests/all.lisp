(uiop:define-package :hw8/tests/all
    (:use :cl
          :hw8/tests/lexer
          :hw8/tests/parser
	      :hw8/tests/macro-characters
		  :hw8/tests/cfg
		  :hw8/tests/dfa)
  (:export #:test-suite))

(in-package :hw8/tests/all)

(defun test-suite ()
  (run-lexer-tests)
  (run-parser-tests)
  (run-macro-characters-tests)
  (run-cfg-tests)
  (run-dfa-tests))
