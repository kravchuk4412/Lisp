"Practice #1 task by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defun make-mapcar-if (clause func1 func2)
   "Function that returns functions for making mapcar-if like:
   (setf test (make-mapcar-if #'oddp #'1+ #'1-))
   (mapcar test '(1 2 3))) -> (2 1 4)." 
   (lambda (arg)
      (if (funcall clause arg)
         (funcall func1 arg)
         (funcall func2 arg))))

;---Task#2--------------------------------------------------------------------------    
(defun my-member (elem lst &key (test #'eql) (key #'identity))
   "Function is analogous to the library function `member'."
   (labels ((%temp (lst)
      (when lst
         (if (funcall test elem (funcall key (first lst)))
            lst
            (%temp (rest lst))))))
            (%temp lst)))

;---Task#3--------------------------------------------------------------------------
(defun my-reduce (func lst &key from-end (initial-value nil initial-value-set-p))
   "Function is analogous to the library function `reduce'."
   (let ((correct-lst 
                  (if initial-value-set-p
                        (if from-end (reverse lst) lst)
                        (if from-end (rest (reverse lst)) (rest lst))))
        (correct-initial-value 
                  (if initial-value-set-p
                        initial-value
                        (if from-end (first (last lst)) (first lst))))) 
      (labels ((%temp (lst-in initial-value-in)   
         (if lst-in
            (if from-end
               (%temp (rest lst-in) (funcall func (first lst-in) initial-value-in))
               (%temp (rest lst-in) (funcall func initial-value-in (first lst-in))))
               initial-value-in)))
         (%temp correct-lst correct-initial-value))))

;---TestPack--------------------------------------------------------------------------
(defun test-make-mapcar-if (name clause func1 func2 input expected)
   (setf test (make-mapcar-if clause func1 func2))
   (let ((result (mapcar test input)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A~%
         Clause, func1, func2: ~A ~A ~A~%Result: ~A~%Expected: ~A~%"
         name input clause func1 func2 result expected))))

(defun test-my-reduce (name func lst key-initial-value key-from-end expected)
   (let ((result (cond
      ((and key-initial-value key-from-end) (my-reduce func lst :initial-value key-initial-value :from-end key-from-end))
      (key-initial-value (my-reduce func lst :initial-value key-initial-value))
      (key-from-end (my-reduce func lst :from-end key-from-end))   
      (t (my-reduce func lst)))))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~A Initial-value: ~A From-end: ~A ~%Result: ~A~%Expected: ~A~%"
               name func lst key-initial-value key-from-end result expected))))

(defun test-my-member (name elem lst key-test key-key expected)
   (let ((result (cond
      ((and key-test key-key) (my-member elem lst :test key-test :key key-key))
      (key-key (my-member elem lst :key key-key))
      (key-test (my-member elem lst :test key-test))   
      (t (my-member elem lst)))))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~A Test: ~A Key: ~A ~%Result: ~A~%Expected: ~A~%" name elem lst key-test key-key result expected))))
 
(defun run-make-mapcar-if-tests ()
   (test-make-mapcar-if "make-mapcar-if-1" #'oddp #'1+ #'1- '(1 2 3) '(2 1 4))
   (test-make-mapcar-if "make-mapcar-if-2" #'listp #'first #'list '(1 (2) 3) '((1) 2 (3)))
   (test-make-mapcar-if "make-mapcar-if-3" (lambda (x) (eq x 0)) #'numberp  #'listp '(0 0 1) '(t t nil)))
   
(defun run-my-member-tests ()
   (test-my-member "my-member-1" 2 '((1 1) (3 3) (2 2)) #'< #'first '((3 3) (2 2)))
   (test-my-member "my-member-2" 2 '((1 1) (3 3) (2 2)) #'> #'first '((1 1) (3 3) (2 2)))
   (test-my-member "my-member-3" 3 '(1 2 3 4 5) nil nil '(3 4 5)))

(defun run-my-reduce-tests ()
   (test-my-reduce "my-reduce-1" #'list '(1 2 3) nil nil (reduce #'list '(1 2 3)))
   (test-my-reduce "my-reduce-2" #'list '(1 2 3) 7 nil (reduce #'list '(1 2 3) :initial-value 7))
   (test-my-reduce "my-reduce-3" #'list '(1 2 3 4) nil t (reduce #'list '(1 2 3 4) :from-end T))
   (test-my-reduce "my-reduce-4" #'list '(1 2 3) 7 t (reduce #'list '(1 2 3) :from-end T :initial-value 7)))
 
(defun run-practice-tests ()
   (run-my-member-tests)
   (run-my-reduce-tests)
   (run-make-mapcar-if-tests))
   
(run-practice-tests)
