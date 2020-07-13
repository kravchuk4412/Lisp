(uiop:define-package :hw8/tests/dfa
    (:use :cl
	  :hw8/lexer-parser/all
          :hw8/cfg/all
	  :hw8/dfa/all)
  (:export #:run-dfa-tests))

(in-package :hw8/tests/dfa)

(defun check-result (name input expected dfa)
  "Fuction that compares two hashtables."
  (let ((result-dfa (build-dfa (create-cfg-map input) dfa))
	(error-flag nil)
	(ex-in-map (first expected))
	(ex-out-map (second expected)))
    (with-accessors ((in-map in-map) (out-map out-map) (cfg-map cfg-map)) result-dfa
      (format t "~%Processing: ~A" name)
      (maphash #'(lambda (key value)
		   (unless (equal (gethash key in-map) (gethash key ex-in-map))
		     (unless error-flag (setf error-flag t))
		     (format t "~%KEY: ~A~%VALUE: ~A" key (node-expr value))
		     (format t "~%IN: ~A" (gethash key in-map))
		     (format t "~%EXPECTED-IN: ~A" (gethash key ex-in-map))
		     (format t "~%"))
		   
		   (unless (equal (gethash key out-map) (gethash key ex-out-map))
		     (unless error-flag (setf error-flag t))
		     (format t "~%KEY: ~A~%VALUE: ~A" key (node-expr value))
		     (format t "~%OUT: ~A" (gethash key out-map))
		     (format t "~%EXPECTED-OUT: ~A" (gethash key ex-out-map))
		     (format t "~%")))
	       cfg-map))
    (if error-flag
	(format t "~%Test: ~A failed.~%" name)
	(format t "~%Test: ~A finished successfully.~%" name))
    t))

(defun fill-table (result-lst map)
  (mapc (lambda (elem)
	  (setf (gethash (first elem) map) (second elem)))
	result-lst))

(defun liveness-expected-result-1 ()
  (let ((dfa-in-map (make-hash-table))
	(dfa-out-map (make-hash-table))
	
	(in-result '((0 nil)
		     (1 (G A C))
		     (2 (G A C))
		     (3 (G A C B))
		     (4 (B))
		     (5 (A C B))
		     (6 (B))
		     (-1 nil)))
	
	(out-result '((0 (G A C))
		      (1 (G A C))
		      (2 (G A C B))
		      (3 (A C B))
		      (4 (B))
		      (5 (B))
		      (6 nil)
		      (-1 nil))))
    
    (fill-table in-result dfa-in-map)
    (fill-table out-result dfa-out-map)
    (list dfa-in-map dfa-out-map)))

;(defun liveness-expected-result-2 () )

;(defun liveness-expected-result-3 () )

(defun reaching-expected-result-1 ()
  (let ((dfa-in-map (make-hash-table))
	(dfa-out-map (make-hash-table))
	
	(in-result '((0 nil)
		     (1 nil)
		     (2 nil)
		     (3 ((B 2)))
		     (4 ((B 2)))
		     (5 ((B 2))) 
		     (6 ((A 4) (B 2)))
		     (-1 ((C 6) (A 4) (B 2)))))
	
	(out-result '((0 nil)
		      (1 nil)
		      (2 ((B 2)))
		      (3 ((B 2)))
		      (4 ((A 4) (B 2)))
		      (5 ((B 2)))
		      (6 ((C 6) (A 4) (B 2)))
		      (-1  nil))))
   
    (fill-table in-result dfa-in-map)
    (fill-table out-result dfa-out-map)
    (list dfa-in-map dfa-out-map)))

;(defun reaching-expected-result-2 () )

;(defun reaching-expected-result-3 () )


(defun run-liveness-tests ()
 (let ((test-1 '($block
		 ($label "l1")
		 ($stmt "const b := 1;")
		 ($if "g"
		   ($stmt "const a := b + 1;")
		   ($stmt "a + c - 1;"))
		 ($stmt "const c := b * 2;")))

	(test-2 '($block
		  ($while "b"
		   ($block
		    ($stmt "c + a + d;")
		    ($stmt "const a := 1;")))))
	
	(test-3 '($block
		  ($if "a"
		   ($block
		    ($stmt "const b := a + k;")
		    ($stmt "const c := a - b;"))
		   ($block
		    ($stmt "const a := b + b;")
		    ($stmt "const g := a - a;"))))))

   (declare (ignore test-2))
   (declare (ignore test-3))

   (check-result "test-liveness-1" test-1 (liveness-expected-result-1) (make-instance 'liveness-analysis))
   ; (check-result "test-liveness-2" test-2 (liveness-expected-result-2) (make-instance 'liveness-analysis))
   ; (check-result "test-liveness-3" test-3 (liveness-expected-result-3) (make-instance 'liveness-analysis))
   ))

  
(defun run-reaching-tests ()
  (let ((test-1 '($block
		  ($label "l1")
		  ($stmt "const b := 1;")
		  ($if "g"
		   ($stmt "const a := b + 1;")
		   ($stmt "a + c - 1;"))
		  ($stmt "const c := b * 2;")))

	(test-2 '($block
		  ($stmt "const b := 3;")
		  ($while "b"
		   ($block
		    ($stmt "const a := a + 1;")
		    ($stmt "const b := a - 1;")))))
	
	(test-3 '($block
		  ($stmt "const a := 3;")
		  ($if "a"
		   ($block
		    ($stmt "const b := a + 1;")
		    ($stmt "const c := a - 1;"))
		   ($block
		    ($stmt "const a := a + 2;")
		    ($stmt "const b := a - 2;"))))))
    (declare (ignore test-2))
    (declare (ignore test-3))
    (check-result "test-reaching-definitions-1" test-1 (reaching-expected-result-1) (make-instance 'reaching-definitions))))
    ;(check-result "test-reaching-definitions-2" test-2 (reaching-expected-result-2) (make-instance 'reaching-definitions))
    ;(check-result "test-reaching-definitions-3" test-3 (reaching-expected-result-3) (make-instance 'reaching-definitions)))
  

(defun run-dfa-tests ()
  (run-liveness-tests)
  (run-reaching-tests))

