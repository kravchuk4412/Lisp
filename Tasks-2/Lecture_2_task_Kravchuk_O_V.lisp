"Lecture #2 task by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defun compose (lst)
   "Function that accepts the entry list of functions
   and returns a function that consistently performs each of the functions
   over the result of the previous one."
   (lambda (arg)
      (reduce #'funcall lst :initial-value arg :from-end t)))

;---Task#2--------------------------------------------------------------------------
(defun my-flatten (some-list)
   "Function that converts a list `some-list' with sub-lists
   into a simple list with elements at one level."
   (reduce (lambda (el acc)
      (if (listp el)
         (reduce #'cons (my-flatten el) :from-end t :initial-value acc)
         (cons el acc)))
      some-list
      :from-end t
      :initial-value nil))
      
;---TestPack--------------------------------------------------------------------------
(defun test-my-flatten (name input expected)
   (let ((result (my-flatten input)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%" name  input result expected))))

(defun test-compose (name func-list input expected)
   (setq composed-fn (compose func-list))
   (let ((result (funcall composed-fn input)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A Function-list: ~A~%Result: ~A~%Expected: ~A~%"
         name input func-list result expected))))
 
(defun run-compose-tests ()
   (test-compose "compose-1" '(1+ 1+) 1 3)
   (test-compose "compose-2" '(1- 1+ 1-) 5 4)
   (test-compose "compose-3" nil 5 5)
   (test-compose "compose-4" '(not not not) t nil))
   
(defun run-my-flatten-tests ()
   (test-my-flatten "my-flatten-1" '(nil) nil)
   (test-my-flatten "my-flatten-2" nil nil)
   (test-my-flatten "my-flatten-3" '(1 () ((2) 3) (((4)))) '(1 2 3 4)))
     
(defun run-lecture_task-tests ()
   (run-compose-tests)
   (run-my-flatten-tests))
   
(run-lecture_task-tests)