"Home task #5 by Oleksandr Kravchuk"

;---Lexical analyzer---------------------------------------------------------
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
   analyze it and return list of tokens.
   Example: (lexer \"2 * (4 - 5) + 1\")
   Result: ((num 2) mul o-parenth (num 4) sub (num 5) c-parenth add (num 1))"
   (let* ((expr-lst (coerce expr-str 'list))
          (separators-lst '((#\+ add) (#\- sub) (#\/ div) 
             (#\* mul) (#\( o-parenth) (#\)  c-parenth)))
          (separators-table (list-to-table separators-lst))
          (whitespaces-lst '((#\Return ret) (#\Space space) (#\Tab tab)))
          (whitespaces-table (list-to-table whitespaces-lst)))
         
      (labels ((%parse-number (lst)
            (when lst 
               (let ((ch (first lst)))
                  (if (digit-char-p ch)
                     (multiple-value-bind (number saved-lst) 
                           (%parse-number (rest lst))
                        (values 
                           (cons ch number)
                           saved-lst))
                     (values
                        nil
                        lst)))))
                        
         (%parse-identifier (lst)
            (when lst 
               (let ((ch (first lst)))
                  (if (alphanumericp ch)
                     (multiple-value-bind (identifier saved-lst) 
                           (%parse-identifier (rest lst))
                        (values 
                           (cons ch identifier)
                           saved-lst))
                     (values
                        nil
                        lst))))) 
                        
         (%lexical-analysis (lst)
            (when lst
               (let ((ch (first lst)))
                  (cond
                     ((gethash ch whitespaces-table)
                        (%lexical-analysis  (rest lst)))
                     ((gethash ch separators-table) 
                        (cons (gethash ch separators-table) (%lexical-analysis (rest lst))))
                     ((digit-char-p ch)
                        (multiple-value-bind (number saved-lst)
                              (%parse-number lst)
                           (cons 
                              (list 'num (parse-integer (coerce number 'string))) 
                              (%lexical-analysis  saved-lst))))
                     ((alpha-char-p ch)
                        (multiple-value-bind (identifier saved-lst)
                              (%parse-identifier lst)
                           (cons 
                              (list 'id (intern (coerce identifier 'string))) 
                              (%lexical-analysis saved-lst))))
                     (t (error (format nil "Unexpected symbol: ~A !" ch))))))))
            (%lexical-analysis expr-lst))))

;---Syntax analyzer-------------------------------------------------------------------  
(defun parser (lexem-lst)
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
            (%expr))
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
                  ((and (listp lexem) lexem) lexem)
                  ((eq lexem 'o-parenth)
                     (let ((expr-result (%expr)))
                        (if (eq (%read-next-lexem) 'c-parenth)
                           expr-result
                           (error "Parser: Expected close's paranthes!"))))
                  (t (error "Parser: Expected number or open's paranthes!")))))
         ;<empty>
         (%empty (lst)
            lst))
         
         (when lst
            (let ((grammar-p (%grammar)))
               (if lst 
                  (error "Parser: Expected sign after number/identifier!")
                  grammar-p))))))

;---Print tree------------------------------------------------------------------------
(defun print-tree (tree-lst)
   "Function that takes result of parser (parse tree) as input
   and returns its string representation."
   (labels ((%expr-p (lst)
         "Checks whether the tree's type is equal to the expression."
         (eq (first lst) 'expr))
      
      (%dump (lst)
         "Check type of current tree and call appropriate function."
         (when lst
            (let ((lst-type (first lst)))
               (cond 
                  ((eq lst-type 'num) (%dump-number lst))
                  ((eq lst-type 'id) (%dump-identifier lst))
                  ((eq lst-type 'expr) (%dump-expr lst))
                  (t (error "Print tree: Incorrect input list!"))))))
               
      (%dump-number (lst)
         "Dump number `(num ...)' in string format."
         (format nil "~S" (second lst)))
       
      (%dump-identifier (lst)
        "Dump identifier `(id ...)' in string format."
         (format nil "~S" (second lst)))
         
      (%dump-expr (lst)
        "Dump expresiion `(expr ...)' in string format."      
         (let* ((op-symbol (second lst))
               (r-oper (%right-operand (third lst) op-symbol))
               (l-oper (%left-operand (fourth lst) op-symbol))
               (op (%dump-operation op-symbol)))
         
         (concatenate 'string r-oper op l-oper)))
      
      (%dump-operation (op-symbol)
         "Dump operation in string format."
         (case op-symbol
            (add "+")
            (sub "-")
            (mul "*")
            (div "/")))
      
      (%right-operand (lst prev-op)
         "Dump right operand in string format and add parentheses if it`s necessary."
         (if (%expr-p lst)
            (let ((op (second lst))) 
               (if (and 
                     (or (eq prev-op 'mul) (eq prev-op 'div)) 
                     (or (eq op 'add) (eq op 'sub)))
                  (concatenate 'string "(" (%dump-expr lst) ")")
                  (%dump-expr lst)))
            (%dump lst)))
      
      (%left-operand (lst prev-op)
         "Dump left operand in string format and add parentheses if it`s necessary."
         (if (%expr-p lst)
            (let ((op (second lst))) 
               (if (or (eq op 'add) 
                       (eq op 'sub) 
                       (eq prev-op 'mul) 
                       (eq prev-op 'div))
                  (concatenate 'string "(" (%dump-expr lst) ")")
                  (%dump-expr lst)))
            (%dump lst))))

      (%dump tree-lst)))
      
;---TestPack--------------------------------------------------------------------------
(defun testing (func name input expected)
   (let ((result (funcall func input)))
         (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A~%Result: ~A~%Expected: ~A~%"
         name input result expected))))
         
(defun run-lexer-tests ()
   (testing #'lexer "lexer-1" "2 *  ( 4 - 5 ) + 1" 
      '((num 2) mul o-parenth (num 4) sub (num 5) c-parenth add (num 1)))
   (testing #'lexer"lexer-2" "(A+2)/(B*4)"
      '(o-parenth (id a) add (num 2) c-parenth div o-parenth (id b) mul (num 4) c-parenth))
   (testing #'lexer "lexer-3" "1+2-3*4/5"
      '((num 1) add (num 2) sub (num 3) mul (num 4) div (num 5)))
   (testing #'lexer "lexer-4" "((2*2)-1)/5"
      '(o-parenth o-parenth (num 2) mul (num 2) c-parenth sub (num 1) c-parenth div (num 5))))

(defun run-parser-tests ()
   (testing #'parser "parser-1" (lexer "2*(4-5)+1") 
      '(expr add (expr mul (num 2) (expr sub (num 4) (num 5))) (num 1)))
   (testing #'parser "parser-2" (lexer "(A+2)/(B*4)")
      '(expr div (expr add (id a) (num 2)) (expr mul (id b) (num 4))))
   (testing #'parser "parser-3" (lexer "1+2-3*4/5")
      '(expr sub (expr add (num 1) (num 2)) (expr div (expr mul (num 3) (num 4)) (num 5))))
   (testing #'parser "parser-4" (lexer "((2*2)-1)/5")
      '(expr div (expr sub (expr mul (num 2) (num 2)) (num 1)) (num 5))))

(defun run-print-tree-tests ()
   (testing #'print-tree "print-tree-1" (parser (lexer "2*(4-5)+1")) "2*(4-5)+1")
   (testing #'print-tree "print-tree-2" (parser (lexer "(3+2)/(4*4)")) "(3+2)/(4*4)")
   (testing #'print-tree "print-tree-3" (parser (lexer "1+2-3*4/5")) "1+2-3*4/5")
   (testing #'print-tree "print-tree-4" (parser (lexer "((2*2)-1)/5")) "(2*2-1)/5")
   (testing #'print-tree "print-tree-5" (parser (lexer "(1-(1-1))")) "1-(1-1)"))
      
(defun run-hometask-tests ()
   (run-lexer-tests)
   (run-parser-tests)
   (run-print-tree-tests))

(run-hometask-tests)   