"Practice #2 task by Oleksandr Kravchuk"

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
            
(defun merge-strings (&rest str-args)
   "Function that implements merge of variable number of input strings like:
   (merge-strings \"one\" \"listp\") -> \"olniestp\""
   (let ((lst-args (reduce 
                        (lambda (el acc)
                           (aif (coerce el 'list)
                              (cons it acc)
                              acc))
                        str-args
                        :from-end t
                        :initial-value nil)))
      (labels ((%temp (args)
         (when args
            (nconc 
               (mapcar #'first args)
               (%temp (reduce 
                        (lambda (el acc)
                           (aif (rest el)
                              (cons it acc)
                              acc))
                        args
                        :from-end t
                        :initial-value nil))))))
         (coerce (%temp lst-args) 'string))))
         
;---Task#2--------------------------------------------------------------------------
(defun archieve-string (str)
   "Function that takes string `str' and replaces sequences of letters
   with letter and length of its sequence. Returns new string."
   (let ((lst-str (coerce str 'list)))
      (labels ((%parse-ch (ch counter lst)
            (labels ((%temp (counter lst)
               (if (and lst (eq ch (first lst)))
                  (multiple-value-bind (number saved-lst) 
                        (%temp (1+ counter) (rest lst))
                     (values 
                        number
                        saved-lst))
                  (values
                     counter
                     lst))))
               (%temp counter lst)))

         (%temp (lst)
            (when lst
               (let ((ch (first lst)))
                  (multiple-value-bind (count saved-lst)
                        (%parse-ch ch 1 (rest lst))
                     (cons ch
                        (append 
                           (when (> count 1)
                              (coerce (format nil "~S" count) 'list))
                           (%temp saved-lst))))))))
         (coerce (%temp lst-str) 'string))))

;---Task#3--------------------------------------------------------------------------
(defun find-greatest-common-substring (str1 str2)
   "Function that takes two strings - `str1' and `str2' -
   and returns the longest common sub-sequence."
   (let ((smaller-str (if (< (length str1) (length str2)) str1 str2))
         (bigger-str (if (>= (length str1) (length str2)) str1 str2)))
      (labels ((%temp (str num)
         (when (>= (length str) num)
            (let ((substr (subseq str 0 num)))
               (if (search substr bigger-str)
                  substr
                  (%temp (subseq str 1) num))))))
         (let ((return-value (do ((i (length smaller-str) (1- i)))
                                 ((<= i 0))
                                 (let ((result (%temp smaller-str i)))
                                    (and result (return result))))))
            (or return-value "")))))

;---Task#4--------------------------------------------------------------------------   
(defun generate-triangle (file &key (depth 5) (max-n 9))
   "Generate triangle of numbers and dump as CSV `file"
   (with-open-file (out file :direction :output :if-exists :supersede)
      (dotimes (level depth)
         (let (numbers)
            (dotimes (_ (1+ level))
               (push (1+ (random max-n)) numbers))
                  (format out "~{~a~^,~}~%" numbers)))))

(defun split-string (str)
   "Function that takes csv-string `str' and
   returns list of elements from this string."
   (let ((pos (search "," str)))
      (if pos
         (cons 
            (parse-integer (subseq str 0 pos)) 
            (split-string (subseq str (1+ pos))))
         (list (parse-integer (subseq str 0))))))
               
(defun split-string (str)
   "Function that takes csv-string `str' and
   returns list of elements from this string."
   (let* ((lst-str (coerce str 'list))
         (pos (position-if (lambda (ch) (eql ch #\,)) lst-str)))
      (if pos
         (cons 
            (parse-integer (coerce (labels ((%temp (lst num)
                        (when (> num 0)
                           (cons (first lst) (%temp (rest lst) (1- num))))))
                        (%temp lst-str pos))
                     'string))
            (split-string (coerce (nthcdr (1+ pos) lst-str) 'string)))
         (list (parse-integer (coerce lst-str 'string))))))
         
(defun read-triangle-from-file (file)
   "Function that takes file-path `file' where triangle is stored
   and returns list of lists (each list is level of triangle)." 
   (let ((result-list))
      (with-open-file (stream file)
         (do ((line (read-line stream) (read-line stream nil 'eof)))
             ((eq line 'eof))
            (push (split-string line) result-list)))
      (reverse result-list)))

(defun create-triangle-tree-list (lst)
   "Function that takes triangle's list `lst'
   and creates new tree-like list."
   (first (reduce (lambda (elem acc)
                     (maplist (lambda (lst1 lst2)
                                 (list (first lst1) (first lst2) (second lst2)))
                              elem
                              acc))
                  lst
                  :from-end t)))

(defun shortest-path (file)
   "Function that takes file-path `file' where triangle is stored
   and returns list with the shortest-path in a triangle.
   Structure of returned list: firts element - sum of path,
                               rest elements - entry path."
   (let ((tree-lst (create-triangle-tree-list (read-triangle-from-file file))))
      (labels ((%traversal (lst)
         ;Temporary function that takes 3-elements list `lst'
         ;and returns list, where first element is sum of the first element in `lst'
         ;and smaller element of the rest. Rest elements in the returned list is path-list.
         (if (not (listp lst))
            (list lst (list lst))
            (let ((second-sum (if (listp (second lst))
                                    (%traversal (second lst))
                                    (list (second lst) (second lst))))
                  (third-sum (if (listp (third lst))
                                    (%traversal (third lst))
                                    (list (third lst) (third lst)))))
               (if (<= (first second-sum) (first third-sum))
                  (cons (+ (first lst) (first second-sum)) (cons (first lst) (rest second-sum))) 
                  (cons (+ (first lst) (first third-sum)) (cons (first lst) (rest third-sum))))))))
      (%traversal tree-lst))))
      
;---Task#5--------------------------------------------------------------------------
(defun greatest-palindrome (str-arg)
   "Function that takes string `str-arg', finds biggest palindrome 
   and returns it. (Palindromes can be even and odd.)"
   (let ((lst-str (coerce str-arg 'list)))
      (labels ((%temp (lst num)
            (when (>= (length lst) num)
               (let ((substr (first-n-elements lst num)))
               ;Or in this way
               ;(let ((substr (nthcdr (- (length lst) num) (reverse lst))))
                  (if (equal substr (reverse substr))
                     (coerce substr 'string)
                     (%temp (rest lst) num))))))
         (do ((i (length lst-str) (1- i)))
             ((<= i 0))
            (let ((result (%temp lst-str i)))
               (and result (return result)))))))

(defun first-n-elements (lst n)
   "Function that takes list `lst' and
   returns a list of the first `n' `lst' items." 
   (labels ((%temp (lst n)
      (when (> n 0)
         (cons 
            (first lst)
            (%temp (rest lst) (1- n))))))
      (if (> n (length lst))
         lst
         (%temp lst n))))
       
;---TestPack------------------------------------------------------------------------
(defun test-merge-strings (name expected &key (str1 "") (str2 "") (str3 ""))
   (let ((result (merge-strings str1 str2 str3)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~A ~A ~%Result: ~A~%Expected: ~A~%"
         name str1 str2 str3 result expected))))

(defun test-archieve-string (name str expected)
   (let ((result (archieve-string str)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%"
         name str result expected))))

(defun test-find-greatest-common-substring (name str1 str2 expected)
   (let ((result (find-greatest-common-substring str1 str2)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~A ~%Result: ~A~%Expected: ~A~%"
         name str1 str2 result expected))))

(defun test-shortest-path (name file expected)
   (let ((result (shortest-path file)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input file: ~A ~%Result: ~A~%Expected: ~A~%"
         name file result expected))))
         
(defun test-greatest-palindrome (name str expected)
   (let ((result (greatest-palindrome str)))
      (or (equal result expected)
      (format t "~%Test ~A failed. Input: ~A ~%Result: ~A~%Expected: ~A~%"
         name str result expected))))
         
(defun run-merge-strings-tests ()
   (test-merge-strings "merge-strings-1" "")
   (test-merge-strings "merge-strings-2" "Follow" :str1 "Follow")
   (test-merge-strings "merge-strings-3" "otnaek" :str1 "one" :str2 "tak")
   (test-merge-strings "merge-strings-4" "loaiks" :str1 "lis" :str2 "ok" :str3 "a"))

(defun run-archieve-string-tests ()
   (test-archieve-string "archieve-string-1" "abc" "abc")
   (test-archieve-string "archieve-string-2" "aaaaa" "a5")
   (test-archieve-string "archieve-string-3" "abbccc" "ab2c3")
   (test-archieve-string "archieve-string-4" "woooohooo" "wo4ho3"))

(defun run-greatest-common-substring-tests ()
   (test-find-greatest-common-substring "find-greatest-common-substring-1" "abc" "def" "")
   (test-find-greatest-common-substring "find-greatest-common-substring-2" "aabbb" "bbbaa" "bbb")
   (test-find-greatest-common-substring "find-greatest-common-substring-3" "onethree" "ethtwo" "eth")
   (test-find-greatest-common-substring "find-greatest-common-substring-4" "abcdf" "aaabcdfff" "abcdf"))

(defun run-shortest-path-tests ()
   (test-shortest-path "shortest-path-1" "Practice-2-Tests\\triangle1.txt" '(19 7 2 5 2 3))
   (test-shortest-path "shortest-path-2" "Practice-2-Tests\\triangle2.txt" '(17 2 8 1 3 3)))
   
(defun run-greatest-palindrome-tests ()
   (test-greatest-palindrome "greatest-palindrome-1" "1221122" "221122")
   (test-greatest-palindrome "greatest-palindrome-2" "aaaaa" "aaaaa")
   (test-greatest-palindrome "greatest-palindrome-3" "avcvcvd" "vcvcv")
   (test-greatest-palindrome "greatest-palindrome-4" "asdafw" "a"))

(defun run-practice-tests ()
   (run-merge-strings-tests)
   (run-archieve-string-tests)
   (run-greatest-common-substring-tests)
   (run-shortest-path-tests)
   (run-greatest-palindrome-tests))
   
(run-practice-tests)