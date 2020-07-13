"Home task #3 by Oleksandr Kravchuk"

;---Table#1--------------------------------------------------------------------------    
(defstruct table1
   id1
   name
   surname
   age)

;---Table#2--------------------------------------------------------------------------    
(defstruct table2
   id2
   auto
   color
   year)

;---Table#3--------------------------------------------------------------------------       
(defstruct table3
   id3
   id1
   id2)
   
;---Table#4--------------------------------------------------------------------------       
(defstruct table4
   id4
   id1
   name
   surname
   age
   id2
   auto
   color
   year)

;---Convert csv line to list --------------------------------------------------------          
(defun split-string (str)
   "Function that takes csv-string `str' and
   returns list of elements from this string."
   (let ((lst-str (coerce str 'list)))
      (labels (
         (%return-token (lst num)
            (when (> num 0)
               (cons 
                  (first lst) 
                  (%return-token (rest lst) (1- num)))))
         (%split-list (lst)
            (let ((pos (position-if (lambda (ch) (eql ch #\,)) lst)))
               (if pos
                  (cons 
                     (coerce (%return-token lst pos) 'string)
                     (%split-list (nthcdr (1+ pos) lst)))
                  (list (coerce lst 'string))))))
         (%split-list lst-str))))

;---Create the table from the file----------------------------------------------------          
(defun dump-csv-to-hash (file struct-func)
   "Function takes two arguments - path to file (`file') and function 
   that creates a record of the desired structure (`struct-func').
   Returns hash-table with the generated structure records from the file."
   (let ((table (make-hash-table :test #'equal)))
      (with-open-file (stream file)
          (do ((line (read-line stream) (read-line stream nil 'eof)))
              ((eq line 'eof))
              (let ((splitted-line (split-string (remove #\Return line))))
                  (setf (gethash (first splitted-line) table) (funcall struct-func splitted-line)))))
      table)) 

;---Structure functions----------------------------------------------------------------    
(defun make-table1-row (lst)
   "Function that based on the list `lst'
   generates and returns record for the table1 structure." 
   (make-table1 
      :id1 (nth 0 lst)
      :name (nth 1 lst)
      :surname (nth 2 lst)
      :age (nth 3 lst)))
    
(defun make-table2-row (lst)
   "Function that based on the list `lst'
   generates and returns record for the table2 structure." 
   (make-table2 
      :id2 (nth 0 lst)
      :auto (nth 1 lst)
      :color (nth 2 lst)
      :year (nth 3 lst)))
     
(defun make-table3-row (lst)
   "Function that based on the list `lst'
   generates and returns record for the table3 structure." 
   (make-table3 
      :id3 (nth 0 lst)
      :id1 (nth 1 lst)
      :id2 (nth 2 lst)))

;---Create Table4----------------------------------------------------------------------   
(defun create-table4 (file1 file2 file3)
   "Function that based on the three files (`file1' `file2' `file3')
   generates and returns hash-table Table4 according to the task." 
   (let ((tab1 (dump-csv-to-hash file1 #'make-table1-row))
         (tab2 (dump-csv-to-hash file2 #'make-table2-row))
         (tab3 (dump-csv-to-hash file3 #'make-table3-row))
         (tab4 (make-hash-table :test #'equal)))
      (maphash (lambda (key value)
                  (let ((tab1-record (gethash (table3-id1 value) tab1))
                       (tab2-record (gethash (table3-id2 value) tab2)))
                     (setf 
                        (gethash key tab4)
                        (make-table4 
                              :id4 key
                              :id1 (table1-id1 tab1-record)
                              :name (table1-name tab1-record)
                              :surname (table1-surname tab1-record)
                              :age (table1-age tab1-record)
                              :id2 (table2-id2 tab2-record)
                              :auto (table2-auto tab2-record)
                              :color (table2-color tab2-record)
                              :year (table2-year tab2-record)))))
                tab3)
      tab4))

;---Finally selector function--------------------------------------------------------------------     
(defun make-select (file1 file2 file3)
   "Function that gets names of three files and returns lambda.
   The lambda makes a selection from Table 1, Table 2 and Table 3 by the specified keys.
   If no keys specified then all the records will be selected.
   The result should be dumped into the result file."
   (let ((tab4 (create-table4 file1 file2 file3)))
      (lambda (res-file &key name surname age auto color year)
         (with-open-file (stream res-file :direction :output :if-exists :supersede)
            (maphash (lambda (key value) (declare (ignore key))
                       (when (and
                              (if name (equal (table4-name value) name) t)
                              (if surname (equal (table4-surname value) surname) t)
                              (if age (equal (table4-age value) age) t)
                              (if auto (equal (table4-auto value) auto) t)
                              (if color (equal (table4-color value) color) t)
                              (if year (equal (table4-year value) year) t))
                           (format stream "~A,~A,~A,~A,~A,~A,~A,~A,~A~%"  (table4-id4 value)
                              (table4-id1 value) (table4-name value) (table4-surname value)
                              (table4-age value) (table4-id2 value) (table4-auto value)
                              (table4-color value) (table4-year value))))
                      tab4)))))
           
;---TestPack--------------------------------------------------------------------------
(defun test-split-string (name str expected)
   (let ((result (split-string str)))
      (or (equalp result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%"
         name str result expected))))

(defun test-make-select (test-name file1 file2 file3 result-file expected-file
                           &key name surname age auto color year)
   (funcall (make-select file1 file2 file3)
      result-file :name name :surname surname :age age :auto auto :color color :year year)
   (let ((result)
         (expected))
         
      (with-open-file (stream result-file)
         (do ((line (read-line stream) (read-line stream nil 'eof)))
             ((eq line 'eof))
            (push line result)))
            
      (with-open-file (stream expected-file)
         (do ((line (read-line stream) (read-line stream nil 'eof)))
             ((eq line 'eof))
            (push line expected)))
   
      (or (equalp result expected)
      (format t "~%Test ~A failed. ~%Input-files:~%~A ~%~A ~%~A ~%~%Result-file: ~A~%Expected-file: ~A~%"
         test-name file1 file2 file3 result-file expected-file))))
      
(defun run-split-string-tests ()
   (test-split-string "split-string-1" "2,3,4,5" '("2" "3" "4" "5"))
   (test-split-string "split-string-2" "2,3,4," '("2" "3" "4" ""))
   (test-split-string "split-string-3" "one" '("one")))

(defun run-make-select-tests ()
   (test-make-select "make-select-1" "Hometask-3-Tests\\table1.txt" "Hometask-3-Tests\\table2.txt"
      "Hometask-3-Tests\\table3.txt" "Hometask-3-Tests\\result-1.txt" "Hometask-3-Tests\\expected-1.txt")
   (test-make-select "make-select-2" "Hometask-3-Tests\\table1.txt" "Hometask-3-Tests\\table2.txt"
      "Hometask-3-Tests\\table3.txt" "Hometask-3-Tests\\result-2.txt" "Hometask-3-Tests\\expected-2.txt"
      :name "Dmytro" :color "Green"))
   
(defun run-hometask-tests ()
  ; (run-make-select-tests)
   (run-split-string-tests))

(run-hometask-tests)   