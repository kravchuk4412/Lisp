"Home task #2 by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defun create-set (args)
   "The following function creates a set with 0 or more elements.
   CREATE-SET returns a lambda (predicate) that gets one parameter. This
   predicate (lets name it CONTAINS) returns true if an element belongs to the
   set and false otherwise."
   (lambda (elem)
      (when (member elem args)
         t)))

(defun my-union (contains-a contains-b)
   "Function that implements union of two sets."
   (lambda (elem)
      (or (funcall contains-a elem) (funcall contains-b elem))))

(defun intersect (contains-a contains-b)
   "Function that implements intersect of two sets."
   (lambda (elem)
      (and (funcall contains-a elem) (funcall contains-b elem))))
         
(defun difference (contains-a contains-b)
   "Function that implements difference of two sets (A - B)."
   (lambda (elem)
      (and (funcall contains-a elem) (not (funcall contains-b elem)))))

;---Task#2--------------------------------------------------------------------------
(defun selection-sort-functional (lst)
   "Selection sort algorithm in functional way."
   (when lst
      (let ((min-elem (apply #'min lst))) 
         (cons min-elem (selection-sort-functional (remove min-elem lst :count 1))))))
         
(defun selection-sort-procedural (lst)
   "Selection sort algorithm in procedural way."
   (let ((min-elem 0)
         (min-pos 0))
      (dotimes (i (- (length lst) 1))
         (setq min-elem (nth i lst))
         (setq min-pos i)
         (do ((j (1+ i) (1+ j)))
            ((>= j (length lst)))
            (when (< (nth j lst) min-elem)
               (setq min-elem (nth j lst))
               (setq min-pos j)))
         (setf (nth min-pos lst) (nth i lst))
         (setf (nth i lst) min-elem)))
   lst)

(defun is-sortedp (lst)
   "Function that returns t if `lst' is sorted
   and nil otherwise."
   (cond 
      ((null (second lst)) t)
      ((< (first lst) (second lst)) (is-sortedp (rest lst)))
      (t nil)))

(defun exchange (lst)
   "Function that implements exchange in exchange sort."
   (cond 
      ((null (second lst)) lst)
      (( < (car lst) (car (cdr lst))) (cons (car lst) (exchange (cdr lst))))
      ( t (cons (second lst) (exchange (cons (car lst) (cddr lst)))))))

(defun exchange-sort-functional (lst)
   "Exchage sort algorithm in functional way."
   (cond 
      ((null (second lst)) lst)
      ((is-sortedp lst) lst)
      (t (exchange-sort-functional (exchange lst)))))
      
(defun exchange-sort-procedural (lst)
   "Exchage sort algorithm in procedural way."
   (let ((buffer 0))
      (do ((r (length lst) (1- r)))
         ((<= r 1))
         (dotimes (i (1- r))
            (when (> (nth i lst) (nth (1+ i) lst))
               (setq buffer (nth i lst))
               (setf (nth i lst) (nth (1+ i) lst))
               (setf (nth (1+ i) lst) buffer)))))
   lst)
  
(defun insert-inner (lst elem)
   "Inner insertion sort algorithm function."
   (cond ((null lst) (cons elem nil))
      ((<= elem (first lst)) (cons elem lst))
      (t (cons (first lst) (insert-inner (rest lst) elem)))))

(defun insertion-sort-functional (lst)
   "Insertion sort algorithm in functional way."
  (when lst 
    (insert-inner (insertion-sort-functional (rest lst)) (first lst)))) 
      
(defun insertion-sort-procedural (lst)
   "Insert sort algorithm in procedural way."
   (let ((buffer 0)
      (j 0))
      (do ((i 1 (1+ i)))
         ((>= i (length lst)))
         (setq buffer (nth i lst))
         (setq j 0)
         (do ()
            ((>= (nth j lst) buffer))
            (setq j (1+ j)))
            
         (do ((k (1- i) (1- k)))
            ((< k j))
            (setf (nth (1+ k) lst) (nth k lst)))
         
         (setf (nth j lst) buffer)))
   lst)
 
;---Task#3--------------------------------------------------------------------------
(defun gen-my-sum ()
   "Function that takes 0 arguments and creates and returns function
   that could be passed to mapcar, so this mapcar will convert list of numbers into
   list of sums of current number and all previous numbers in the original list."
   (let ((buf 0))
      (lambda (elem)
         (setq buf (+ buf elem)))))

;---Task#4--------------------------------------------------------------------------
 (defun create-eraser (k)
   "Function that takes number parameter `k' and creates and returns function that:
   1) takes list as input argument;
   2) returns list of elements that occur in input list more than or equal to k times.
   The resulting list shouldn't contain any duplicates."
   (lambda (lst)
      (labels ((%temp (lst-arg)
         (when lst-arg
            (if (>= (count (first lst-arg) lst-arg) k)
               (cons (first lst-arg) (%temp (remove (first lst-arg) lst-arg)))
               (%temp (remove (first lst-arg) lst-arg))))))
         (%temp lst))))
 
;---Task#5--------------------------------------------------------------------------
(defun factorial (num)
   "Function that implements factorial (without data validation)." 
   (if (> num 1)
      (* num (factorial (1- num)))
      1))

(defun combinatorics-formula (m-elem n-places)
   "Function that returns the result of the formula: 
   C = n! / (m! * (n-m)!)."
   (/ (factorial n-places) (* (factorial m-elem) (factorial (- n-places m-elem)))))
   
(defun num-orders (lst-arg) 
   "Function that takes list of numbers (amount of thread operations) 
   and return number of all possible operation execution orders. Usage example:
   (num-orders '(1 1)) => 2;
   (num-orders '(1 2)) => 3;
   (num-orders '(1 1 1)) => 6."
   (let ((number-of-operation (apply #'+ lst-arg)))
      (labels ((%temp (lst num)
         (if (rest lst)
            (* (combinatorics-formula (first lst) num) (%temp (rest lst) (- num (first lst))))
            1)))
         (if lst-arg
            (%temp lst-arg number-of-operation)
            0))))
 
;---Task#6--------------------------------------------------------------------------
(defun my-q-sort (lst)
   "Function that implements quicksort in lisp."
   (if (rest lst) 
      (let ((main-elem (first lst))
            (left-lst ())
            (right-lst ()))
         (dolist (cur-elem (rest lst))
            (if (> main-elem cur-elem) 
               (push cur-elem left-lst)
               (push cur-elem right-lst)))
         (append (my-q-sort left-lst) (list main-elem) (my-q-sort right-lst)))
      lst))
 
;---TestPack--------------------------------------------------------------------------
(defun test-create-set (name input-set input-elem expected)
   (let ((result (funcall (create-set input-set) input-elem)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-set: ~A  Input-elem: ~A ~%Result: ~A~%Expected: ~A~%"
            name input-set input-elem result expected))))

(defun test-set-operations (operation name set-1 set-2 elem expected)
   (let ((result (funcall (funcall operation (create-set set-1) (create-set set-2)) elem)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Set-1: ~A  Set-2: ~A  Elem: ~A~%Result: ~A~%Expected: ~A~%"
            name set-1 set-2 elem result expected))))

(defun test-gen-my-sum (name input-list expected)
   (let ((result (mapcar (gen-my-sum) input-list)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-list: ~A~%Result: ~A~%Expected: ~A~%"
            name input-list result expected))))

(defun test-create-eraser (name input-k input-list expected)
   (let ((result (funcall (create-eraser input-k) input-list)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-k: ~A Input-list: ~A~%Result: ~A~%Expected: ~A~%"
            name input-k input-list result expected))))
            
(defun test-factorial (name input-number expected)
   (let ((result (factorial input-number)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-number: ~A~%Result: ~A~%Expected: ~A~%"
            name input-number result expected))))

(defun test-num-orders (name input-list expected)
   (let ((result (num-orders input-list)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-list: ~A~%Result: ~A~%Expected: ~A~%"
            name input-list result expected))))

(defun test-sort-algorithms (sort-algorithm name input-list expected)
   (let ((result (funcall sort-algorithm input-list)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-list: ~A~%Result: ~A~%Expected: ~A~%"
            name input-list result expected))))
            
(defun run-create-set-tests ()
   (test-create-set "create-set-1" nil 5 nil)
   (test-create-set "create-set-2" '(10) 10 t)
   (test-create-set "create-set-3" '(15 13) 5 nil)
   (test-create-set "create-set-4" '(a b c) 'b t))
   
(defun run-my-union-tests ()
   (test-set-operations #'my-union "my-union-1" nil nil 10 nil)
   (test-set-operations #'my-union "my-union-2" nil '(10 11 12) 11 t)
   (test-set-operations #'my-union "my-union-3" '(15 13) '(15 14) 13 t)
   (test-set-operations #'my-union "my-union-4" '(a b c) '(d) 'e nil))

(defun run-intersect-tests ()
   (test-set-operations #'intersect "intersect-1" nil nil nil nil)
   (test-set-operations #'intersect "intersect-2" nil '(10 11 12) 11 nil)
   (test-set-operations #'intersect "intersect-3" '(15 13) '(15 14) 15 t)
   (test-set-operations #'intersect "intersect-4" '(a b c) '(d) 'd nil))

(defun run-difference-tests ()
   (test-set-operations #'difference "difference-1" nil nil 3 nil)
   (test-set-operations #'difference "difference-2" '(10 11 12) '(11) 11 nil)
   (test-set-operations #'difference "difference-3" '(15 13) '(15 14) 13 t)
   (test-set-operations #'difference "difference-4" '(a b) '(d c) 'd nil))

(defun run-gen-my-sum-tests ()
   (test-gen-my-sum "gen-my-sum-1" nil nil)
   (test-gen-my-sum "gen-my-sum-2" '(1) '(1))
   (test-gen-my-sum "gen-my-sum-3" '(1 2) '(1 3))
   (test-gen-my-sum "gen-my-sum-4" '(1 2 3 4) '(1 3 6 10)))

(defun run-create-eraser-tests ()
   (test-create-eraser "create-eraser-1" 0 nil nil)
   (test-create-eraser "create-eraser-2" 0 '(1 2 3 4 5) '(1 2 3 4 5))
   (test-create-eraser "create-eraser-3" 2 '(1 2 2 3 7 7 7) '(2 7))
   (test-create-eraser "create-eraser-4" 5 '(1 2 3) nil))

(defun run-factorial-tests ()
   (test-factorial "factorial-1" 0 1)
   (test-factorial "factorial-2" 1 1)
   (test-factorial "factorial-3" 2 2)
   (test-factorial "factorial-4" 5 120))

(defun run-num-orders-tests ()
   (test-num-orders "num-orders-1" '(1 1) 2)
   (test-num-orders "num-orders-2" '(1 1 1) 6)
   (test-num-orders "num-orders-3" '(1 2) 3)
   (test-num-orders "num-orders-4" nil 0)
   (test-num-orders "num-orders-4" '(2 2 2) 90))

(defun run-my-q-sort-tests ()
   (test-sort-algorithms #'my-q-sort "my-q-sort-1" nil nil)
   (test-sort-algorithms #'my-q-sort "my-q-sort-2" '(1) '(1))
   (test-sort-algorithms #'my-q-sort "my-q-sort-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'my-q-sort "my-q-sort-4" '(3 3 3 3 2 2) '(2 2 3 3 3 3)))
   
(defun run-selection-sort-functional-tests ()
   (test-sort-algorithms #'selection-sort-functional "selection-sort-functional-1" nil nil)
   (test-sort-algorithms #'selection-sort-functional "selection-sort-functional-2" '(1) '(1))
   (test-sort-algorithms #'selection-sort-functional "selection-sort-functional-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'selection-sort-functional "selection-sort-functional-4" '(3 3 3 3 1 1 2) '(1 1 2 3 3 3 3)))
   
(defun run-selection-sort-procedural-tests ()
   (test-sort-algorithms #'selection-sort-procedural "selection-sort-procedural-1" nil nil)
   (test-sort-algorithms #'selection-sort-procedural "selection-sort-procedural-2" '(1) '(1))
   (test-sort-algorithms #'selection-sort-procedural "selection-sort-procedural-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'selection-sort-procedural "selection-sort-procedural-4" '(3 3 3 3 2 2) '(2 2 3 3 3 3)))
   
(defun run-exchange-sort-functional-tests ()
   (test-sort-algorithms #'exchange-sort-functional "exchange-sort-functional-1" nil nil)
   (test-sort-algorithms #'exchange-sort-functional "exchange-sort-functional-2" '(1) '(1))
   (test-sort-algorithms #'exchange-sort-functional "exchange-sort-functional-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'exchange-sort-functional "exchange-sort-functional-4" '(1 3 2 8) '(1 2 3 8)))
   
(defun run-exchange-sort-procedural-tests ()
   (test-sort-algorithms #'exchange-sort-procedural "exchange-sort-procedural-1" nil nil)
   (test-sort-algorithms #'exchange-sort-procedural "exchange-sort-procedural-2" '(1) '(1))
   (test-sort-algorithms #'exchange-sort-procedural "exchange-sort-procedural-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'exchange-sort-procedural "exchange-sort-procedural-4" '(3 3 3 3 2 2) '(2 2 3 3 3 3)))
   
(defun run-insertion-sort-functional-tests ()
   (test-sort-algorithms #'insertion-sort-functional "insert-sort-functional-1" nil nil)
   (test-sort-algorithms #'insertion-sort-functional "insert-sort-functional-2" '(1) '(1))
   (test-sort-algorithms #'insertion-sort-functional "insert-sort-functional-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'insertion-sort-functional "insert-sort-functional-4" '(3 3 3 3 2 2) '(2 2 3 3 3 3)))
   
(defun run-insertion-sort-procedural-tests ()
   (test-sort-algorithms #'insertion-sort-procedural "insert-sort-procedural-1" nil nil)
   (test-sort-algorithms #'insertion-sort-procedural "insert-sort-procedural-2" '(1) '(1))
   (test-sort-algorithms #'insertion-sort-procedural "insert-sort-procedural-3" '(5 4 3 2 1) '(1 2 3 4 5))
   (test-sort-algorithms #'insertion-sort-procedural "insert-sort-procedural-4" '(3 3 3 3 2 2) '(2 2 3 3 3 3)))
   
(defun run-hometask-tests ()
   (run-create-set-tests)
   (run-my-union-tests)
   (run-intersect-tests)
   (run-difference-tests)
   (run-gen-my-sum-tests)
   (run-create-eraser-tests)
   (run-factorial-tests)
   (run-num-orders-tests)
   (run-my-q-sort-tests)
   (run-selection-sort-functional-tests)
   (run-selection-sort-procedural-tests)
   (run-exchange-sort-functional-tests)
   (run-exchange-sort-procedural-tests)
   (run-insertion-sort-functional-tests)
   (run-insertion-sort-procedural-tests))

(run-hometask-tests)