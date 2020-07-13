"Home task #1 by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defun sum (lst)
   "Function that finds the sum of elements from the list of arguments `lst'."
   (if lst
      (+ (first lst) (sum (rest lst)))
      0))

;---Task#2--------------------------------------------------------------------------         
(defun inc-lst (lst)
   "Function that accepts the list of numbers `lst' 
   and returns a new list with incremental numbers."
   (when lst
      (cons (1+ (first lst)) (inc-lst (rest lst)))))

;---Task#3--------------------------------------------------------------------------      
(defun my-reverse (lst)
   "Function that checks whether `lst' is a list.
   Calls `my-reverse-inner' if everything is OK,
   otherwise returns NIL."
   (and (listp lst) 
         lst
         (my-reverse-inner lst nil)))

(defun my-reverse-inner (lst res)
   "Function that returns the list `lst' in reverse order (create new list)."
   (if lst
      (my-reverse-inner (rest lst) (cons (first lst) res))
      res))

;---Task#4--------------------------------------------------------------------------
(defun delete-repeats (lst)
   "Function that returns the list `lst' without duplicates.
   Saves only the last found instance."
   (when lst
      (if (member (first lst) (rest lst)) 
         (delete-repeats (rest lst))
         (cons (first lst) (delete-repeats (rest lst))))))
         
;---TestPack--------------------------------------------------------------------------
(defun test-func (func name input expected)
   (let ((result (funcall func input)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%" name  input result expected))))
 
(defun test-my-reverse-inner (name lst res expected)
   (let ((result (my-reverse-inner lst res)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~A ~%Result: ~A~%Expected: ~A~%" name  lst res result expected))))
      
(defun run-sum-tests ()
   (test-func #'sum "sum-1" '(1) 1)
   (test-func #'sum "sum-2" '(1 2 3 4 5) 15)
   (test-func #'sum "sum-3" nil 0))
   
(defun run-inc-lst-tests ()
   (test-func #'inc-lst "inc-lst-1" '(1) '(2))
   (test-func #'inc-lst "inc-lst-2" '(1 2 3) '(2 3 4))
   (test-func #'inc-lst "inc-lst-3" nil nil))

(defun run-my-reverse-tests ()
   (test-func #'my-reverse "my-reverse-1" '(1 2 3) '(3 2 1))
   (test-func #'my-reverse "my-reverse-2" '(1) '(1))
   (test-func #'my-reverse "my-reverse-3" nil nil)
   (test-func #'my-reverse "my-reverse-4" 'a nil))
 
(defun run-my-reverse-inner-tests ()
   (test-my-reverse-inner "my-reverse-inner-1" '(1 2 3) nil '(3 2 1))
   (test-my-reverse-inner "my-reverse-inner-2" '(1 2) '(3 4) '(2 1 3 4))
   (test-my-reverse-inner "my-reverse-inner-3" nil nil nil)
   (test-my-reverse-inner "my-reverse-inner-4" nil '(1 2) '(1 2)))
   
(defun run-delete-repeats-tests ()
   (test-func #'delete-repeats "delete-repeats-1" '(2 2 2) '(2))
   (test-func #'delete-repeats "delete-repeats-2" '(1 2 2 3 3 4 4 4) '(1 2 3 4))
   (test-func #'delete-repeats "delete-repeats-3" nil nil)
   (test-func #'delete-repeats "delete-repeats-4" '(1 2 2 3 3 2 1) '(3 2 1)) 
   (test-func #'delete-repeats "delete-repeats-5" '(nil nil nil) '(nil))) 
     
(defun run-hometask-tests ()
   (run-sum-tests)
   (run-inc-lst-tests)
   (run-my-reverse-tests)
   (run-my-reverse-inner-tests)
   (run-delete-repeats-tests))
   
(run-hometask-tests)