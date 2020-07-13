"Lecture #5 task by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defmacro aif (expr t-body &optional f-body)
   "Implemention of macro `aif': (aif (expr) (foo it) (bar)), that works
   the same way as  if, but result of the `(expr)' is implicitly bound to symbol `it'."
   (let ((res-name (gensym)))
      `(let ((,res-name ,expr))
         (if ,res-name 
            (let ((it ,res-name))
               ,t-body)
            ,f-body))))

;---Task#2--------------------------------------------------------------------------
(defmacro awhen (expr &body body)
   "Implemention of macro `awhen': (awhen (expr) ... (foo it)), that works
   the same way as when, but result of the `expr' is implicitly bound to `it'."
   `(let ((it ,expr))
      (when it
         ,@body)))
      
;---Task#3--------------------------------------------------------------------------
(defmacro cut (&rest rest)
   "This macro expand into a function, which takes number of arguments
   equal to the number of '_' in the passed forms,
   and pass those arguments to the corresponding positions in the passed forms."
   (let ((args-lst))
      (labels ((%fill-in-the-gaps (lst)
         (mapcar (lambda (elem)
                     (cond 
                        ((listp elem) (%fill-in-the-gaps elem))
                        ((eq elem '_) (push (gensym) args-lst) (first args-lst))
                        (t elem)))
                     lst)))
         (let ((full-rest (%fill-in-the-gaps rest)))
            `(lambda (,@(reverse args-lst)) ,full-rest)))))

;---Task#4--------------------------------------------------------------------------
(defmacro our-let (par-lst &body body)
   "Implementiof macro our-let which works in the same way as standard let."
   (let ((parametrs-lst (mapcar (lambda (elem) 
                                    (if (listp elem) 
                                       (first elem) 
                                       elem)) 
                                 par-lst))
         (values-lst (mapcar (lambda (elem) 
                                 (when (listp elem) 
                                    (second elem))) 
                              par-lst)))
   `((lambda (,@parametrs-lst) ,@body) ,@values-lst)))

;---TestPack--------------------------------------------------------------------------
(defun test-macro (name expected result)
   (or (equal result expected)
   (format t "~%Test ~A failed.~%Result: ~A~%Expected: ~A~%"
      name result expected)))
         
(defun run-aif-tests ()
   (test-macro "aif-1" 6 (aif (+ 1 1) (+ it 4)))
   (test-macro "aif-2" 1 (aif nil 3 (+ 0 1)))
   (test-macro "aif-3" 'abc (aif 'abc it)))
   
(defun run-awhen-tests ()
   (test-macro "awhen-1" 5 (awhen (+ 1 1) (+ 1 4)))
   (test-macro "awhen-2" 3 (awhen (- 4 2) (+ it 1)))
   (test-macro "awhen-3" nil (awhen nil 'a)))

(defun run-our-let-tests ()
   (test-macro "our-let-1" '(nil 1 nil) (our-let (a (b 1) (c)) (list a b c)))
   (test-macro "our-let-2" nil  (our-let ((a 1))))
   (test-macro "our-let-3" 7  (our-let ((a 1) (b 2) (c 4)) (+ a b c))))
   
(defun run-lecture-task-tests ()
   (run-aif-tests)
   (run-awhen-tests)
   (run-our-let-tests))
   
(run-lecture-task-tests)