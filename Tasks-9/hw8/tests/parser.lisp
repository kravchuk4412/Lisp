(uiop:define-package :hw8/tests/parser
    (:use :cl
	  :hw8/lexer-parser/all)
  (:export #:run-parser-tests))

(in-package :hw8/tests/parser)

(defun check-result (name input expected)
  (let ((result (parser (lexer input))))
    (or (equal result expected)
	(format t "~%Test ~A failed. Input: ~A~%Result: ~A~%Expected:
         ~A~%" name input result expected))))

(defun run-parser-tests ()
   (check-result "parser-1" "2*(4-5)+1;" 
      '(grammar stmt (stmt expr (expr add (expr mul (expr num (num 2)) (expr sub (expr num (num 4)) (expr num (num 5)))) (expr num (num 1))))))
   (check-result "parser-2" "(A+2)/(B*4);"
       '(grammar stmt (stmt expr (expr div (expr add (expr id (id a)) (expr num (num 2))) (expr mul (expr id (id b)) (expr num (num 4)))))))
   (check-result "parser-3" "1+2-3*4/5;"
       '(grammar stmt (stmt expr (expr sub (expr add (expr num (num 1)) (expr num (num 2))) (expr div (expr mul (expr num (num 3)) (expr num (num 4))) (expr num (num 5)))))))
   (check-result "parser-4" "((2*2)-1)/5;"
	     '(grammar stmt (stmt expr (expr div (expr sub (expr mul (expr num (num 2)) (expr num (num 2))) (expr num (num 1))) (expr num (num 5))))))
  (check-result "parser-5" "const a := 4+5+a;" 
	    '(grammar stmt (stmt definition (definition assign (id a) (expr add (expr add (expr num (num 4)) (expr num (num 5))) (expr id (id a))))))))
