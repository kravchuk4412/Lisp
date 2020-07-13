"Lecture #3 task by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defun merge-strings (str1 str2)
   "Function that implements merge of two strings like:
   (merge-strings \"one\" \"listp\") -> \"olniestp\""
   (let ((lst1 (coerce str1 'list))
         (lst2 (coerce str2 'list)))
   (labels ((%temp (lst-str1 lst-str2)
      (cond
         ((null lst-str2) lst-str1)
         ((null lst-str1) lst-str2)
         (t (cons (first lst-str1) (cons (first lst-str2) (%temp (rest lst-str1) (rest lst-str2))))))))
            (coerce (%temp lst1 lst2) 'string))))

;---Task#2--------------------------------------------------------------------------    
(defun string-to-hash-table (str)
   "Function that converts the input string into a hash table that
   stores all positions for each character from the line.
   The symbol is key. Return hash table."
   (let ((table (make-hash-table)))
      (reduce (lambda (pos elem) 
               (setf (gethash elem table) (cons pos (gethash elem table)))
            (1+ pos)) 
            str 
            :initial-value 0)
         table))

;---Task#3--------------------------------------------------------------------------
(defun get-string-length-from-hash-table (hash-table)
   "Function that counts and returns length of future string." 
   (let ((str-len ()))
      (maphash (lambda (key value) 
            (push (length value) str-len)) 
         hash-table)
      (apply #'+ str-len)))

(defun hash-table-to-string (hash-table)
   "Function that generates an initial string from the table,
   obtained as a result of performing the function from the second task."
   (let ((str (make-string (get-string-length-from-hash-table hash-table))))
      (maphash (lambda (key value) 
            (mapcar (lambda (pos) (setf (char str pos) key)) value)) 
         hash-table)
      str))
      
;---TestPack--------------------------------------------------------------------------
(defun test-merge-strings (name str1 str2 expected)
   (let ((result (merge-strings str1 str2)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~A ~%Result: ~A~%Expected: ~A~%" name str1 str2 result expected))))

(defun test-string-to-hash-table (name str expected)
   (let ((result (string-to-hash-table str)))
      (or (equalp result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%" name str result expected))))

(defun test-table-to-string (name str expected)
   (let ((result (hash-table-to-string (string-to-hash-table str))))
      (or (equalp result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%" name str result expected))))
   
(defun run-merge-strings-tests ()
   (test-merge-strings "merge-strings-1" nil nil "")
   (test-merge-strings "merge-strings-2" "Follow" "me" "Fmoellow")
   (test-merge-strings "merge-strings-3" "me" "Follow" "mFeollow")
   (test-merge-strings "merge-strings-4" "abc" "123" "a1b2c3"))

(defun run-string-to-hash-table-tests ()
   (let ((expected_table (make-hash-table)))
   (setf (gethash #\s expected_table) '(7 6 0))
   (setf (gethash #\o expected_table) '(1))
   (setf (gethash #\m expected_table) '(4 2))
   (setf (gethash #\e expected_table) '(5 3))
   (test-string-to-hash-table "string-to-hash-table-1" "somemess" expected_table)))

(defun run-table-to-string-tests ()
   (test-table-to-string "table-to-string-1" "onetwo" "onetwo")
   (test-table-to-string "table-to-string-2" "test" "test" )
   (test-table-to-string "table-to-string-3" nil ""))
     
(defun run-lecture-task-tests ()
   (run-string-to-hash-table-tests)
   (run-table-to-string-tests)
   (run-merge-strings-tests))
   
(run-lecture-task-tests)