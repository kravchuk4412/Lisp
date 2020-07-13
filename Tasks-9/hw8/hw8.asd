(asdf:defsystem "hw8"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :description "System for home work #8-9 (macro-characters and cfg)"
  :version "0.2.0"
  :author "Oleksandr Kravchuk"
  :depends-on ("hw8/lexer-parser/all"
	       "hw8/macro-characters/all"
	       "hw8/cfg/all"
	       "hw8/dfa/all"
	       "hw8/task/all")
  :in-order-to ((test-op (load-op "hw8/tests/all")))
  :perform (test-op (o c) (uiop:symbol-call :hw8/tests/all :test-suite)))
