(asdf:defsystem "hw9"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :description "System for home work #9 (CFG)"
  :version "0.1.0"
  :author "Oleksandr Kravchuk"
  :depends-on ("hw9/cfg/all")
  :in-order-to ((test-op (load-op "hw9/tests/all")))
  :perform (test-op (o c) (uiop:symbol-call :hw9/tests/all :test-suite)))
