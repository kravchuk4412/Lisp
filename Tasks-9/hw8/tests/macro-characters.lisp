(uiop:define-package :hw8/tests/macro-characters
    (:use :cl
	  :hw8/utils
          :hw8/macro-characters/all
	  :hw8/lexer-parser/all)
  (:export #:run-macro-characters-tests))
  
(in-package :hw8/tests/macro-characters)

(defun check-result (name input expected)
  (or (equal input expected)
      (format t "~%Test ~A failed. ~%Result: ~A~%Expected: ~A~%" name input expected)))

(defun run-macro-characters-tests ()
  (check-result "Test-slash-macro-1"
	  (/. #! expr "1 + 2 + 3" ((expr ?e1 ?e2))
	  (#! expr "?e1 + ?e2" (declare (ignore ?e1)) (declare (ignore ?e2)) #! expr "?e1 - ?e2"))
	  '(EXPR SUB (EXPR SUB (EXPR NUM (NUM 1)) (EXPR NUM (NUM 2))) (EXPR NUM (NUM 3))))
      (check-result "Test-slash-macro-2"
	       (/. #! expr "15 * (16 * A + 16 * B)" ((expr ?e) (id ?i1 ?i2))
		 (#! expr "?e * ?i1 + ?e * ?i2"
		     (declare (ignore ?e))
		     (declare (ignore ?i1))
		     (declare (ignore ?i2))  #! expr "?e * (?i1 + ?i2)"))
	  '(EXPR MUL (EXPR NUM (NUM 15)) (EXPR MUL (EXPR NUM (NUM 16)) (EXPR ADD (EXPR ID (ID A)) (EXPR ID (ID B))))))
  (check-result "Test-sharp-macro-1"
	   (forthis #! num "1"
	     ((num ?n))
	     (#! num "?n" ?n))
	   '(NUM 1))
  (check-result "Test-sharp-macro-2"
	   (forthis #! num "1"
	     ((num ?e))
	     (#! num "?e" (declare (ignore ?e)) #! expr "?e"))
	   '(EXPR NUM (NUM 1)))
  (check-result "Test-sharp-macro-3"
	   (forthis #! id "A"
	     ((id ?i))
	     (#! id "?i" ?i))
	   '(ID A))
  (check-result "Test-sharp-macro-4"
	   (forthis #! id "A"
	     ((id ?i))
	     (#! id "?i" (declare (ignore ?i)) #! expr "?i"))
	   '(EXPR ID (ID A)))
  (check-result "Test-sharp-macro-5"
	   (forthis #! expr "1+2+3"
	     ((expr ?e))
	     (#! expr "?e" ?e))
	   '(EXPR ADD (EXPR ADD (EXPR NUM (NUM 1)) (EXPR NUM (NUM 2))) (EXPR NUM (NUM 3))))
  (check-result "Test-sharp-macro-6"
	   (forthis #! expr "1+2+(3+4)"
	     ((expr ?e))
	     (#! expr "1+2+?e" ?e))
	   '(EXPR ADD (EXPR NUM (NUM 3)) (EXPR NUM (NUM 4))))
  (check-result "Test-sharp-macro-7"
	   (forthis #! expr "1+2+(3+4)"
	     ((expr ?e)
	      (num ?n))
	     (#! expr "1+?n+?e" (declare (ignore ?e)) ?n))
	   '(NUM 2))
  (check-result "Test-sharp-macro-8"
	   (forthis #! expr "1+(A+4)"
	     ((ID ?a))
	     (#! expr "1+(?a+3)" ?a))
	   NIL)
  (check-result "Test-sharp-macro-9"
	   (forthis #! expr "1+((1+A)+4)"
	     ((ID ?a))
	     (#! expr "1+((1+?a)+4)" ?a))
	   '(ID A))
  (check-result "Test-sharp-macro-10"
	   (forthis #! definition "const A := 1+2"
	     ((id ?i))
	     (#! definition "const ?i := 1+2" ?i))
	   '(ID A))
  (check-result "Test-sharp-macro-11"
	   (forthis #! definition "const A := 1+2"
	     ((id ?i))
	     (#! definition "const ?i := 1" ?i))
	   NIL)
  (check-result "Test-sharp-macro-12"
	   (forthis #! definition "const A := 1"
	     ((definition ?d))
	     (#! definition "?d" ?d))
	   '(DEFINITION ASSIGN (ID A) (EXPR NUM (NUM 1))))
  (check-result "Test-sharp-macro-13"
	   (forthis #! definition "const A := 1+2"
	     ((expr ?e))
	     (#! definition "const A := ?e" ?e))
	   '(EXPR ADD (EXPR NUM (NUM 1)) (EXPR NUM (NUM 2))))
  (check-result "Test-sharp-macro-14"
	   (forthis #! stmt "const A := 1;"
	     ((stmt ?s))
	     (#! stmt "?s" ?s))
	   '(STMT DEFINITION (DEFINITION ASSIGN (ID A) (EXPR NUM (NUM 1)))))
  (check-result "Test-sharp-macro-15"
	   (forthis #! stmt "const A := 1;"
	     ((definition ?d))
	     (#! stmt "?d" ?d))
	   '(DEFINITION ASSIGN (ID A) (EXPR NUM (NUM 1))))
  (check-result "Test-sharp-macro-16"
	   (forthis #! stmt "const A := B;"
	     ((expr ?e))
	     (#! stmt "const A := ?e;" ?e))
	   '(EXPR ID (ID B)))
  (check-result "Test-sharp-macro-17"
	   (forthis #! stmt "1+2;"
	     ((expr ?e))
	     (#! stmt "?e" ?e))
	   '(EXPR ADD (EXPR NUM (NUM 1)) (EXPR NUM (NUM 2))))
  (check-result "Test-sharp-macro-18"
	   (forthis #! stmt "1+2;"
	     ((expr ?e))
	     (#! stmt "?e" (declare (ignore ?e)) #! stmt "?e"))
	   '(STMT EXPR (EXPR ADD (EXPR NUM (NUM 1)) (EXPR NUM (NUM 2))))))
