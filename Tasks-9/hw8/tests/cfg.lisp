(uiop:define-package :hw8/tests/cfg
    (:use :cl
	  :cl-graph
	  :hw8/cfg/all)
  (:export #:run-cfg-tests))

(in-package :hw8/tests/cfg)

(defun check-result (name input expected)
  "Fuction that compares two graphs in string-dot-format."
  (let ((result (cfg-to-dot (cfg-to-cl-graph (create-cfg-map input)))))
    (or (equal result expected)
	(format t "~%Test ~A failed. Input: ~A~%Result: ~A~%Expected:~A~%" name input result expected))))

(defun expected-result-1 ()
  (let* ((test-graph (make-graph 'graph-container :default-edge-type :directed))
	;; Add some vertices
	 (line-0 (add-vertex test-graph "ENTRY"))
	 (line-1 (add-vertex test-graph '($label "l1")))
	 (line-2 (add-vertex test-graph '($stmt "const b := a + 1;")))
	 (line-3 (add-vertex test-graph '($while "a")))
	 (line-4 (add-vertex test-graph '($stmt "a = a + 1;")))
	 (line-5 (add-vertex test-graph '($stmt "a = a - 1;")))
	 (line-6 (add-vertex test-graph '($stmt "const a := b * 2;")))
	 (line-7 (add-vertex test-graph '($if "a")))
	 (line-8 (add-vertex test-graph '($goto "l1")))
	 (line-10 (add-vertex test-graph "EXIT"))
	 (line-9 (add-vertex test-graph '($stmt "c;"))))

    ;; Edges
    (add-edge-between-vertexes test-graph line-0 line-1)
    (add-edge-between-vertexes test-graph line-1 line-2)
    (add-edge-between-vertexes test-graph line-2 line-3)
    (add-edge-between-vertexes test-graph line-5 line-3)
    (add-edge-between-vertexes test-graph line-3 line-4)
    (add-edge-between-vertexes test-graph line-4 line-5)
    (add-edge-between-vertexes test-graph line-3 line-6)
    (add-edge-between-vertexes test-graph line-6 line-7)
    (add-edge-between-vertexes test-graph line-7 line-8)
    (add-edge-between-vertexes test-graph line-8 line-1)
    (add-edge-between-vertexes test-graph line-8 line-10)
    (add-edge-between-vertexes test-graph line-7 line-9)
    (add-edge-between-vertexes test-graph line-9 line-10)
		      
    (cfg-to-dot test-graph)))

(defun expected-result-2 ()
  (let* ((test-graph (make-graph 'graph-container :default-edge-type :directed))
	;; Add some vertices
	 (line-0 (add-vertex test-graph "ENTRY"))
	 (line-1 (add-vertex test-graph '($while "a")))
	 (line-2 (add-vertex test-graph '($stmt "a = a + 1;")))
	 (line-3 (add-vertex test-graph '($stmt "a = a - 1;")))
	 (line-4 (add-vertex test-graph "EXIT")))

    ;; Edges
    (add-edge-between-vertexes test-graph line-0 line-1)
    (add-edge-between-vertexes test-graph line-3 line-1)
    (add-edge-between-vertexes test-graph line-1 line-2)
    (add-edge-between-vertexes test-graph line-2 line-3)
    (add-edge-between-vertexes test-graph line-1 line-4)
		      
    (cfg-to-dot test-graph)))

(defun expected-result-3 ()
  (let* ((test-graph (make-graph 'graph-container :default-edge-type :directed))
	;; Add some vertices
	 (line-0 (add-vertex test-graph "ENTRY"))
	 (line-1 (add-vertex test-graph '($if "a")))
	 (line-2 (add-vertex test-graph '($stmt "a = a + 1;")))
	 (line-3 (add-vertex test-graph '($stmt "a = a - 1;")))
	 (line-6 (add-vertex test-graph "EXIT"))
	 (line-4 (add-vertex test-graph '($stmt "a = a + 2;")))
	 (line-5 (add-vertex test-graph '($stmt "a = a - 2;"))))

    ;; Edges
    (add-edge-between-vertexes test-graph line-0 line-1)
    (add-edge-between-vertexes test-graph line-1 line-2)
    (add-edge-between-vertexes test-graph line-2 line-3)
    (add-edge-between-vertexes test-graph line-3 line-6)
    (add-edge-between-vertexes test-graph line-5 line-6)
    (add-edge-between-vertexes test-graph line-1 line-4)
    (add-edge-between-vertexes test-graph line-4 line-5)
    
    (cfg-to-dot test-graph)))

(defun expected-result-4 ()
  (let* ((test-graph (make-graph 'graph-container :default-edge-type :directed))
	;; Add some vertices
	 (line-0 (add-vertex test-graph "ENTRY"))
	 (line-1 (add-vertex test-graph '($label "l1")))
	 (line-2 (add-vertex test-graph '($stmt "const b := a + 1;")))
	 (line-3 (add-vertex test-graph '($goto "l1")))
	 (line-4 (add-vertex test-graph "EXIT")))
    
    ;; Edges
    (add-edge-between-vertexes test-graph line-0 line-1)
    (add-edge-between-vertexes test-graph line-3 line-1)
    (add-edge-between-vertexes test-graph line-1 line-2)
    (add-edge-between-vertexes test-graph line-2 line-3)
    (add-edge-between-vertexes test-graph line-3 line-4)
		      
    (cfg-to-dot test-graph)))

(defun expected-result-5 ()
  (let* ((test-graph (make-graph 'graph-container :default-edge-type :directed))
	;; Add some vertices
	 (line-0 (add-vertex test-graph "ENTRY"))
	 (line-1 (add-vertex test-graph '($goto "l1")))
	 (line-2 (add-vertex test-graph '($stmt "const b := a + 1;")))
	 (line-3 (add-vertex test-graph '($label "l1")))
	 (line-4 (add-vertex test-graph "EXIT")))

    
    ;; Edges
    (add-edge-between-vertexes test-graph line-0 line-1)
    (add-edge-between-vertexes test-graph line-1 line-2)
    (add-edge-between-vertexes test-graph line-2 line-3)
    (add-edge-between-vertexes test-graph line-1 line-3)
    (add-edge-between-vertexes test-graph line-3 line-4)
		      
    (cfg-to-dot test-graph)))
  
(defun run-cfg-tests ()
  (let ((test-1 '($block
		  ($label "l1")
		  ($stmt "const b := a + 1;")
		  ($while "a"
		   ($block
		    ($stmt "a = a + 1;")
		    ($stmt "a = a - 1;")))
		  ($stmt "const a := b * 2;")
		  ($if "a"
		   ($goto "l1")
		   ($stmt "c;"))))

	(test-2 '($block
		  ($while "a"
		   ($block
		    ($stmt "a = a + 1;")
		    ($stmt "a = a - 1;")))))
	
	(test-3 '($block
		  ($if "a"
		   ($block
		    ($stmt "a = a + 1;")
		    ($stmt "a = a - 1;"))
		   ($block
		    ($stmt "a = a + 2;")
		    ($stmt "a = a - 2;")))))

	(test-4 '($block
		  ($label "l1")
		  ($stmt "const b := a + 1;")
		  ($goto "l1")))
	
	(test-5  '($block
		   ($goto "l1")
		   ($stmt "const b := a + 1;")
		   ($label "l1"))))

    (check-result "test-cfg-graph-1" test-1 (expected-result-1))
    (check-result "test-cfg-graph-2" test-2 (expected-result-2))
    (check-result "test-cfg-graph-3" test-3 (expected-result-3))
    (check-result "test-cfg-graph-4" test-4 (expected-result-4))
    (check-result "test-cfg-graph-5" test-5 (expected-result-5))))
