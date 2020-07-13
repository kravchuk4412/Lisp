(uiop:define-package :hw8/tests/lexer
    (:use :cl
	  :hw8/lexer-parser/all)
  (:export #:run-lexer-tests))

(in-package :hw8/tests/lexer)

(defun check-result (name input expected)
  (let ((result (lexer input)))
    (or (equal result expected)
	(format t "~%Test ~A failed. Input: ~A~%Result: ~A~%Expected:
         ~A~%" name input result expected))))
         
(defun run-lexer-tests ()
  (check-result "lexer-1" "2 *  ( 4 - 5 ) + 1;" 
	   '((num 2) mul o-parenth (num 4) sub (num 5) c-parenth add (num 1) semicolon))
  (check-result "lexer-2" "(A+2)/(B*4)"
	   '(o-parenth (id A) add (num 2) c-parenth div o-parenth (id B) mul (num 4) c-parenth))
  (check-result "lexer-3" "const a := 1+2-3*4/5"
	   '((keyword const) (id A) assign (num 1) add (num 2) sub (num 3) mul (num 4) div (num 5)))
  (check-result "lexer-4" "((2*2)-1)/5"
	   '(o-parenth o-parenth (num 2) mul (num 2) c-parenth sub (num 1) c-parenth div (num 5))))
