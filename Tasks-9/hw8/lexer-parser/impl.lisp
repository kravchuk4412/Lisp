(uiop:define-package :hw8/lexer-parser/impl
    (:use :cl)
  (:export #:lexer
	   #:add
	   #:sub
	   #:div
	   #:mul
	   #:semicolon
	   #:keyword
	   #:num
	   #:id
	   #:tmpl
	   #:o-parenth
	   #:c-parenth
	   #:assign
	   #:const
	   #:A
	   #:B
	   #:C
	   #:G
	   #:D
	   #:grammar
	   #:stmt
	   #:definition
	   #:expr
	   #:parser
	   #:parse-num
	   #:parse-definition
	   #:parse-stmt
	   #:parse-id
	   #:parse-expr
	   #:parse-grammar
	   #:?a
	   #:?e
	   #:?i
	   #:?n
	   #:?s
	   #:?d
	   #:?e1
	   #:?i1
	   #:?n1
	   #:?e2
	   #:?i2
	   #:?n2))

(in-package :hw8/lexer-parser/impl)

(defun list-to-table (lst)
  "Function that returns hash-table created from list `lst'.
   List should have the following structure ((<key> <value>) ... )."
  (let ((table (make-hash-table)))
    (mapc 
     (lambda (elem)
       (setf (gethash (first elem) table) (second elem)))
     lst)
    table))
      
(defun lexer (expr-str)
  "Function thar takes string with expression as an input,
   analyze it and return list of tokens."
  (let* ((expr-lst (coerce expr-str 'list))
         (separators-lst '((#\+ add) (#\- sub) (#\/ div)
			   (#\* mul) (#\( o-parenth) (#\)  c-parenth) (#\; semicolon)))
         (separators-table (list-to-table separators-lst))
         (whitespaces-lst '((#\Return ret) (#\Space space) (#\Tab tab)))
         (whitespaces-table (list-to-table whitespaces-lst))
	 (keywords-lst '(const)))
         
    (labels ((%parse-lexeme (lst test)
	       (labels ((%parse-lexeme-inner (lst)
			  (when lst 
			    (let ((ch (first lst)))
			      (if (funcall test ch)
				  (multiple-value-bind (lexeme saved-lst) 
				      (%parse-lexeme-inner (rest lst))
				    (values (cons ch lexeme) saved-lst))
				  (values nil lst))))))
		 (%parse-lexeme-inner lst)))
	     
	     (%convert-to-symbol (char-lst &optional prefix)
		   (intern (string-upcase (coerce  (if prefix (cons prefix char-lst) char-lst) 'string)) :hw8/lexer-parser/impl))
                        
             (%lexical-analysis (lst)
               (when lst
		 (let ((ch (first lst)))
                   (cond
                     ((gethash ch whitespaces-table)
                      (%lexical-analysis (rest lst)))
		     ((eq ch #\:)
		      (if (eq (second lst) #\=)
			  (cons 'assign (%lexical-analysis (cddr lst)))
			  (error (format nil "Expected '=' after ':' symbol"))))
                     ((gethash ch separators-table) 
                      (cons (gethash ch separators-table) (%lexical-analysis (rest lst))))
                     ((digit-char-p ch)
                      (multiple-value-bind (number saved-lst)
                          (%parse-lexeme lst #'digit-char-p)
			(cons
			 (list 'num (parse-integer (coerce number 'string))) 
			 (%lexical-analysis  saved-lst))))
		     ((eq ch #\?)
                      (multiple-value-bind (identifier saved-lst)
                          (%parse-lexeme (rest lst) #'alphanumericp)
                        (cons
                         (list 'tmpl
                               (%convert-to-symbol identifier ch)) 
			 (%lexical-analysis saved-lst))))
                     ((alpha-char-p ch)
                      (multiple-value-bind (identifier saved-lst)
                          (%parse-lexeme lst #'alphanumericp)
			(let ((lexeme (%convert-to-symbol identifier)))
			  (cons
			   (if (member lexeme keywords-lst)
			       (list 'keyword lexeme)
			       (list 'id lexeme))
			   (%lexical-analysis saved-lst)))))
                     (t (error (format nil "Unexpected symbol: ~A !" ch))))))))
      (%lexical-analysis expr-lst))))

(defun parser (lexem-lst &optional start-with)
   "Function that takes output of lexer (list of tokens)
   as an input, parses it and return parse tree.
   Parse tree format: (<kind-of-node> <op> (<operand-1>) (<operand-2>))."
   (let ((lst lexem-lst))
      (labels (
         ;Function that returns next lexem 
         (%read-next-lexem ()
            (when lst
               (pop lst)))
         ;<grammar>         
         (%grammar ()
	   (labels ((%grammar-loop ()
		      (when lst
			(cons (%stmt) (%grammar-loop)))))
	     (cons 'grammar (cons 'stmt (%grammar-loop)))))
	 ;<stmt>
	 (%stmt ()
	   (let* ((lexem (%read-next-lexem))
		  (result (progn
			      (push lexem lst)
			      (if (and (listp lexem) (eq (second lexem) 'const))
			       (list 'stmt 'definition (%definition))
			       (list 'stmt 'expr (%expr))))))
	     (if (eq (%read-next-lexem) 'semicolon)
		 result
		 (error "Expected semicolon!"))))
     	 ;<definition>
	 (%definition ()
	   (let ((const-lexem (%read-next-lexem))
		 (id-lexem (%id))
		 (assign-lexem (%read-next-lexem)))
	     (if (and (listp const-lexem) (eq (second const-lexem) 'const))
		 (if (eq assign-lexem 'assign)
			 (cons 'definition (cons 'assign (cons id-lexem (cons (%expr) nil))))
			 (error "Expected assign sign ':=' !"))
	         (error "Expected a 'const' specifier!"))))
         ;<expr>
         (%expr ()
            (%expr-list (%term)))
         ;<term>
         (%term ()
            (%term-list (%factor)))
         ;<expr-list>
         (%expr-list (res-lst)
            (let ((op (%read-next-lexem)))
               (cond 
                  ((or (eq op 'add) (eq op 'sub))
                     (%expr-list (list 'expr op res-lst (%term))))
                  (t (when op (push op lst)) (%empty res-lst)))))
         ;<term-list>
         (%term-list (res-lst)
            (let ((op (%read-next-lexem)))
               (cond 
                  ((or (eq op 'mul) (eq op 'div))
                     (%term-list (list 'expr op res-lst (%factor))))
                  (t (when op (push op lst)) (%empty res-lst)))))
         ;<factor>
         (%factor ()
            (let ((lexem (%read-next-lexem)))
               (cond
		 ((and (listp lexem) lexem)
		  (if (eq (first lexem) 'tmpl)
		      lexem
		      (prog2
			  (push lexem lst)
			  (if (eq (first lexem) 'num)
			      (list 'expr 'num (%num))
			      (list 'expr 'id (%id))))))
                  ((eq lexem 'o-parenth)
                     (let ((expr-result (%expr)))
                        (if (eq (%read-next-lexem) 'c-parenth)
                           expr-result
                           (error "Parser: Expected close's paranthes!"))))
                  (t (error "Parser: Expected number or open's paranthes!")))))
	 ;<num>
	 (%num ()
	   (let ((lexem (%read-next-lexem)))
	     (if (and (listp lexem) (eq (first lexem) 'num))
		 lexem 
		 (error "Expected number!"))))
	 ;<id>
	 (%id ()
	   (let* ((lexem (%read-next-lexem))
		  (kind (if (listp lexem)
			    (first lexem)
			    (error "Expected identifier!"))))
      
	     (cond
	       ((eq kind 'id) lexem)
	       ((eq kind 'tmpl) lexem)
	       (t (error "Expected identifier!")))))
         ;<empty>
         (%empty (lst)
            lst))
         
         (when lst
	   (let ((rules-p (case start-with
			    (stmt (%stmt))
			    (definition (%definition))
			    (expr (%expr))
			    (num (%num))
			    (id (%id))
			    (otherwise (%grammar)))))
               (if lst 
                  (error "Parser: parsing ended, but list with program doesn`t empty!")
                  rules-p))))))

(defun parse-id (str)
  (parser (lexer str) 'id))

(defun parse-expr (str)
  (parser (lexer str) 'expr))

(defun parse-num (str)
  (let* ((lexer-result-lst (lexer str))
	 (lstr (first lexer-result-lst))
         (fst-lstr (first lstr))
         (snd-lstr (second lstr)))
    (if (and (null (rest lexer-result-lst))
	     (symbolp fst-lstr)
	     (case fst-lstr
               (num (numberp snd-lstr))
               (tmpl (symbolp snd-lstr))))
        lstr
        (error "Not a valid number!"))))

(defun parse-definition (str)
  (let* ((def-lst (lexer str))
	(fst-lst (first def-lst)))
    (if (and (listp fst-lst) (eq (first fst-lst) 'tmpl) (null (cdr def-lst)))
	     fst-lst
	     (parser def-lst 'definition))))

(defun parse-stmt (str)
    (let* ((stmt-lst (lexer str))
	   (fst-lst (first stmt-lst)))
    (if (and (listp fst-lst) (eq (first fst-lst) 'tmpl) (null (cdr stmt-lst)))
	     fst-lst
	     (parser stmt-lst 'stmt))))

(defun parse-grammar (str)
  (let* ((grammar-lst (lexer str))
	 (fst-lst (first grammar-lst)))
    (if (and (listp fst-lst) (eq (first fst-lst) 'tmpl) (null (cdr grammar-lst)))
	fst-lst
	(parser grammar-lst))))
