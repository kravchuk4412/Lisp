(uiop:define-package :hw8/dfa/all
    (:use :hw8/dfa/impl)
  (:nicknames :dfa)
  (:export #:dfa
	   #:reaching-definitions
	   #:liveness-analysis
	   #:build-dfa
	   #:in-map
	   #:out-map
	   #:cfg-map))
