"Practice #4 task by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defmacro my-or (&rest exprs)
   "Implemention of macro `my-or', that works the same way as `or' does."
   (let ((result (gensym))) 
      (labels ((%or-exprs (lst)
         (when lst 
            `(let ((,result ,(first lst)))
               (if ,result
                  ,result
                  ,(%or-exprs (rest lst)))))))
         (%or-exprs exprs))))

;---Task#2--------------------------------------------------------------------------
(defmacro my-let* (par-lst &body body)
   "Implemention of macro `my-let*' via `let'-s, that works the same way as `let*' does."
   (labels ((%let-loop (lst)
      (if lst 
        `(let (,(first lst)) ,(%let-loop (rest lst)))
        `(progn ,@body))))
      (%let-loop par-lst)))

;---Task#3--------------------------------------------------------------------------      
(defmacro my-let** (par-lst &body body)
   "Implemention of macro `my-let**' via `lambda'-s, that works the same way as `let*' does."
   (labels ((%lambda-loop (lst)
      (if lst
         (let ((next-let (first lst)))
            (if (listp next-let)
               `((lambda (,(first next-let)) ,(%lambda-loop (rest lst))) ,(second next-let))
               `((lambda (,next-let) ,(%lambda-loop (rest lst))) nil)))
         `(progn ,@body))))
      (%lambda-loop par-lst)))

;---Task#4--------------------------------------------------------------------------
(defmacro alambda (args &body body)
   "Implemention of macro `alambda', that works as local recursive function."
  `(labels ((self ,args ,@body))
     #'self))

;---TestPack------------------------------------------------------------------------
(defun test-macro (name expected result)
   (or (equal result expected)
   (format t "~%Test ~A failed.~%Result: ~A~%Expected: ~A~%"
      name result expected)))
      
(defun run-my-or-tests ()
   (test-macro "my-or-1" nil (my-or))
   (test-macro "my-or-2" nil (my-or (> 2 3) (= 2 3)))
   (test-macro "my-or-3" 5 (my-or (= 1 2) (+ 2 3) (* 2 2)))
   (test-macro "my-or-4" t (my-or (= 1 1) (+ 2 3) (* 2 2))))

(defun run-my-let*-tests ()
   (test-macro "my-let*-1" nil (my-let* ()))
   (test-macro "my-let*-2" 3 (my-let* ((a 1) (b a) (c b)) (+ a b c)))
   (test-macro "my-let*-3" '(nil nil) (my-let* (a (b a)) (list a b))))

(defun run-my-let**-tests ()
   (test-macro "my-let**-1" nil (my-let** ()))
   (test-macro "my-let**-2" 3 (my-let** ((a 1) (b a) (c b)) (+ a b c)))
   (test-macro "my-let**-3" '(nil nil) (my-let** (a (b a)) (list a b))))
   
(defun run-alambda-tests ()
   (test-macro "alambda-1" 24 (funcall (alambda (x) 
                                          (if (= x 0)
                                              1
                                              (* x (self (1- x)))))
                                       4))
   (test-macro "alambda-2" 10 (funcall (alambda (lst) 
                                          (if lst
                                             (+ (first lst) (self (rest lst)))
                                             0))
                                       '(1 2 3 4))))      
                               
(defun run-practice-tests ()
   (run-my-or-tests)
   (run-my-let*-tests)
   (run-my-let**-tests)
   (run-alambda-tests))
   
(run-practice-tests)