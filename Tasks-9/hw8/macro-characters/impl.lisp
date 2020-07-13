(uiop:define-package :hw8/macro-characters/impl
    (:use :cl
	  :hw8/lexer-parser/all
	  :hw8/utils)
  (:export #:/.
	   #:parse-str))

(in-package :hw8/macro-characters/impl)

(defun parse-str (term str)
  (case term
    (grammar (parse-grammar str))
    (stmt (parse-stmt str))
    (definition (parse-definition str))
    (expr (parse-expr str))
    (num (parse-num str))
    (id (parse-id str))))

(set-dispatch-macro-character #\# #\!
                              (lambda (stream char1 char2)
                                (declare (ignore char1 char2))
                                (let ((term (read stream t nil t))
				       (str (read stream t nil t)))
				   
                                  (unless
                                      (and (symbolp term)
                                           (stringp str))
                                    (error "#! has the following format: #! <symbol> <string>"))
				  
				  (quote-tree (parse-str term str) term))))

(defmacro |/.| (term vars &body rules)
  (let ((result (gensym))
	(term-lst (gensym)))
    `(labels ((%while-forthis (,term-lst)
		(let ((,result
			(mapcar (lambda (node)
				  (if (listp node)
				      (%while-forthis node) node))
				,term-lst)))
		     (or
		      (forthis ,result ,vars ,@rules)
		      (if (equal ,result ,term-lst) ,term-lst ,result)))))
	  (%while-forthis ,term))))
