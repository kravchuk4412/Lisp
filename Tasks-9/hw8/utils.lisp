(uiop:define-package :hw8/utils
    (:use :cl
	  :hw8/lexer-parser/all)
  (:export #:forthis
	   #:quote-tree
	   #:aif))

(in-package :hw8/utils)

(defvar *context* nil)

(defstruct var-info
  kind
  val)

(defmacro aif (cnd expr1 &optional expr2)
  (let ((tmp (gensym)))
    `(let ((,tmp ,cnd))
       (if ,tmp
           (let ((it ,tmp))
             ,expr1)
           ,expr2))))


(defun find-in-context (symb &optional (current *context*))
  ;;(format t "find-in-context ~A ~A ~A~%" symb (symbol-package symb) current)
  (if (null current)
      nil
      (aif (gethash symb (car current))
           (progn
             ;(format t "found ~A ~A~%" symb it)
             it)
           (find-in-context symb (cdr current)))))

(defun get-value (symb &optional node-name)
  (aif (find-in-context symb)
       (let 
           ((value (var-info-val it))
            (kind (var-info-kind it)))
       
	   (if node-name
             (case kind
	       (grammar (case node-name
			  (grammar value)
			  (otherwise (error "Something wrong in grammar"))))
	       (stmt (case node-name
		       (grammar (list 'grammar 'stmt value))
		       (stmt value)
		       (otherwise (error "Something wrong in stmt"))))
	       (definition (case node-name
			     (grammar (list 'grammar 'stmt (list 'stmt 'definition value)))
			     (stmt (list 'stmt 'definition value))
			     (definition value)
			     (expr (error "Could not be definition inside the expression!"))
			     (num (error "Could not be definition inside the number!"))
			     (id (error "Could not be definition inside the identifier!"))))
	       (id (case node-name
		     (id value)
		     (expr (list 'expr 'id value))
		     (num (error "Could not be identifier inside the number!"))
	             (stmt (error "Could not be number inside the statement!"))))
               (expr (case node-name
                       (expr value)
		       (grammar (list 'grammar 'stmt (list 'stmt 'expr value)))
		       (stmt (list 'stmt 'expr value))
		       (id (error "Could not be expression inside the identifier!"))
                       (num (error "Could not be expression inside the numbers!"))))
               (num (case node-name
                      (num value)
		      (expr (list 'expr 'num value))
		      (id (error "Could not be number inside the identifier!"))
		      (definition  (error "Could not be number inside the definition!"))
	              (stmt (error "Could not be number inside the statement!")))))
             kind))
       (error "Variable ~A is unbound!" symb)))

(defun add-value (symb value)
  ;(format t "add-value ~A ~A~%" symb value)
  (setf (gethash symb (car *context*)) value))

(defmacro letvar (vars &body body)
  "letvar implementation"
  (let ((new-context (gensym)))
    `(let* ((,new-context (make-hash-table :test #'eq))
            (*context* (cons ,new-context *context*)))
       ,@(mapcar (lambda (var)
                   `(add-value ',(second var)
                               (make-var-info :kind ',(first var) :val ,(third var)))) vars)
       (let ,(forthis-var-helper vars 'get-value)
         ,@body))))

(defun forthis-var-helper (vars fun)
  (mapcan (lambda (vars-of-kind)
            (remove-if
             #'null
             (mapcar
              (lambda (var)
                (if (symbolp var)
		    ;;(intern (symbol-name var) :HW8/LEXER-PARSER/IMPL)
                    (case fun
                      (add-value
                       `(add-value ',var
                                   (make-var-info :kind ',(car vars-of-kind)
                                                  :val ',var)))
                      (get-value `(,var (get-value ',var ',(car vars-of-kind)))))))
              (cdr vars-of-kind))))
          vars))

(defmacro forthis (term vars &body body)
  (let ((trm (gensym))
        (new-context (gensym)))
    `(let* ((,trm ,term)
            (,new-context (make-hash-table :test #'eq))
            (*context* (cons ,new-context *context*)))
       ,@(forthis-var-helper vars 'add-value)
       (cond ,@(mapcar
                (lambda (cnd)
                  `((aif (match ,(car cnd) ,trm)
                         (progn (mapc (lambda (var)
                                        (add-value (first var)
                                                   (make-var-info
                                                    :kind (get-value (first var))
                                                    :val (second var))))
                                      it)
                                (let ,(forthis-var-helper vars 'get-value)
                                  ,@(cdr cnd))))))
                body)))))

(defun quote-tree (tree current-node)
  (cond
    ((listp tree)
     (let ((node-kind (first tree)))
       (if (and (eq node-kind 'stmt) (eq (second tree) 'expr))
	   (list 'list
		 (quote-tree (first tree) node-kind)
		 (quote-tree (second tree) node-kind)
		 (quote-tree (third tree) 'stmt))
	   (if (eq node-kind 'definition)
	       (list 'list
		     (quote-tree (first tree) node-kind)
		     (quote-tree (second tree) node-kind)
		     (quote-tree (third tree) 'id)
		     (quote-tree (fourth tree) 'expr))
	       (if (eq node-kind 'tmpl)
		   (list 'get-value (list 'quote (second tree)) (list 'quote current-node))
		   (cons 'list (mapcar (lambda (tree-node)
					 (quote-tree tree-node (first tree)))
				       tree)))))))
    ((symbolp tree) (list 'quote tree))
    ((numberp tree) tree)))

(defmacro with-unique-names (names &body body)
  `(let (,@(mapcar (lambda (name) (list name `',(gensym))) names))
     ,@body))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-unique-names (val foundp)
        (destructuring-bind ((test &rest progn) &rest others) clauses
          `(multiple-value-bind (,val ,foundp) ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (list x y) binds) t))
   ((varsym? y) (values (cons (list y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun myassoc (x binds)
  ;;(format t "myassoc ~A ~A~%" x binds)
  (when binds
    (if (eq x (first (car binds)))
        (progn
          ;;(format t "myassoc found ~A~%"(second (car binds)))
          (car binds))
        (myassoc x (cdr binds)))))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (myassoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cadr b) b)))) ;;fixed from cdr to cadr
