(uiop:define-package :hw8/dfa/impl
    (:use :cl
          :hw8/lexer-parser/all
	  :hw8/macro-characters/all
          :hw8/utils
	  :hw8/cfg/all)
  (:export #:dfa
	   #:reaching-definitions
           #:liveness-analysis
	   #:build-dfa
	   #:in-map
	   #:out-map
	   #:cfg-map))

(in-package :hw8/dfa/impl)

(defstruct dfa-node
  in
  out)

(defclass dfa ()
  ((cfg-map
    :initarg :cfg-map
    :initform (make-hash-table)
    :accessor cfg-map)
   (in-map
    :initarg :in-map
    :initform (make-hash-table)
    :accessor in-map)
   (out-map
    :initarg :out-map
    :initform (make-hash-table)
    :accessor out-map)
   (gen-map
    :initarg :gen-map
    :initform (make-hash-table)
    :accessor gen-map)
   (kill-map
    :initarg :kill-map
    :initform (make-hash-table)
    :accessor kill-map)))


(defclass liveness-analysis (dfa) ())

(defclass reaching-definitions (dfa) ())

(defgeneric dfa-initialization (dfa-kind &optional cfg-map)
  (:documentation "Initialize `dfa' object (each slot) by calling appropriate functions."))

(defgeneric in-initialization (dfa-kind)
  (:documentation "Initialize `in-map' slot of `dfa' object."))

(defgeneric out-initialization (dfa-kind)
  (:documentation "Initializes `out-map' slot of `dfa' object."))

(defgeneric gen-initialization (dfa-kind)
  (:documentation "Initialize `gen-map' slot of `dfa' object."))

(defgeneric kill-initialization (dfa-kind)
  (:documentation "Initializes `kill-map' slot of `dfa' object."))

(defgeneric dfa-processing (dfa-kind)
  (:documentation "Passes `dfa' object to processing functions and returns modified `dfa' object."))

(defgeneric point-processing (dfa-kind id)
    (:documentation "Passes `dfa' object and identifier of code's point `id' 
                     to processing functions and returns `dfa-node' with result of processing."))

(defgeneric join (dfa-kind id)
  (:documentation "Joins incoming branches and returns result (doesn't change `dfa' object)."))

(defgeneric transfer (dfa-kind id join-set)
  (:documentation "Return result of transfer through code's point (doesn't change `dfa' object)."))

(defgeneric kill-remove (dfa-kind id join-set)
  (:documentation "Contains of `remove' function that is appropriate to current `dfa' object.
                   Returns rusalt list."))


(defmethod in-initialization ((dfa-kind dfa))
  (with-accessors ((cfg-map cfg-map) (in-map in-map)) dfa-kind
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (setf (gethash key in-map) nil))
	     cfg-map)))


(defmethod out-initialization ((dfa-kind dfa))
  (with-accessors ((cfg-map cfg-map) (out-map out-map)) dfa-kind
    (maphash #'(lambda (key value)
		 (declare (ignore value))
		 (setf (gethash key out-map) nil))
	     cfg-map)))

(defun kill-substracting (dfa id join-set)
  "Result of substracting join-set and kill-set."
  (with-accessors ((kill-map kill-map)) dfa
    (let ((kill (gethash id kill-map)))
      (kill-remove dfa kill join-set))))

;--------REACHING DEFINITIONS--------------------------------------------


(defmethod gen-initialization ((dfa-kind reaching-definitions))
  (with-accessors ((cfg-map cfg-map) (gen-map gen-map)) dfa-kind
    (maphash #'(lambda (key value)
		 (if (and (> (node-id value) 0) (eq (first (node-expr value)) '$stmt))
		     (let* ((expr-str (second (node-expr value)))
			    (expr-lst (parse-str 'stmt expr-str))
			    (gen-variable (forthis expr-lst
					      ((id ?i) (expr ?e))
					    (#! stmt "const ?i := ?e;"
						(declare (ignore ?e)) (second ?i)))))
		       
		       (setf (gethash key gen-map)
			     (when gen-variable (list gen-variable (node-id value)))))
		     (setf (gethash key gen-map) nil)))
	     cfg-map)))

(defmethod kill-initialization ((dfa-kind reaching-definitions))
  (with-accessors ((gen-map gen-map) (kill-map kill-map)) dfa-kind
    (maphash #'(lambda (key value)
		 (setf (gethash key kill-map) (first value)))
	     gen-map)))


(defmethod dfa-initialization ((dfa-kind dfa) &optional (cfg-map nil cfg-map-supplied-p))
  (with-accessors ((cfg cfg-map)) dfa-kind
    (when cfg-map-supplied-p (setf cfg cfg-map)))
  (in-initialization dfa-kind)
  (out-initialization dfa-kind)
  (gen-initialization dfa-kind)
  (kill-initialization dfa-kind))


(defmethod kill-remove ((dfa-kind reaching-definitions) kill join-set)
  (remove kill join-set :key #'car))


(defmethod join ((dfa-kind reaching-definitions) id)
  (with-accessors ((cfg-map cfg-map) (out-map out-map)) dfa-kind
    (let* ((predecessors (node-parents (gethash id cfg-map)))
	   (in-list (reduce (lambda (acc parent-id)
				(reduce #'cons
					(gethash parent-id out-map)
					:from-end t
					:initial-value acc))
			    predecessors
			    :initial-value nil)))
      (remove-duplicates in-list :test #'equal))))


(defmethod transfer ((dfa-kind reaching-definitions) id join-set)
  (with-accessors ((cfg-map cfg-map) (gen-map gen-map)) dfa-kind
    (let* ((in-after-kill (kill-substracting dfa-kind id join-set))
	   (gen-variable (gethash id gen-map)))
      (if (or (not gen-variable) (member gen-variable in-after-kill :test #'equal))
	  in-after-kill
	  (cons gen-variable in-after-kill)))))


(defmethod point-processing ((dfa-kind reaching-definitions) id)
  (let* ((join-set (unless (= id +global-entry-id+) (join dfa-kind id)))
	 (transfer-set (unless (= id +global-exit-id+) (transfer dfa-kind id join-set))))
    (make-dfa-node
     :in join-set
     :out transfer-set)))


(defmethod dfa-processing ((dfa-kind reaching-definitions))
  (forward-traversal dfa-kind #'point-processing)
  dfa-kind)

;--------LIVENESS ANALYSIS--------------------------------------------

(defmethod gen-initialization ((dfa-kind liveness-analysis))
  (with-accessors ((cfg-map cfg-map) (gen-map gen-map)) dfa-kind
    (labels ((%collect-used-variables (expr)
	       (let ((used-variables))
		 (/. expr
		     ((id ?i))
		   (#! expr "?i" (push (second ?i) used-variables)))
	       used-variables)))
	     
      (maphash #'(lambda (key value)
		   (setf (gethash key gen-map)
			 (when (> (node-id value) 0)
			   (let ((expr-type (first (node-expr value)))
				 (expr-str (second (node-expr value))))
			     (case expr-type
			       ($stmt (let ((expr-lst (parse-str 'stmt expr-str)))
					(forthis expr-lst
					    ((id ?i) (expr ?e))
					  (#! stmt "const ?i := ?e;" (declare (ignore ?i)) (%collect-used-variables ?e))
					  (#! stmt "?e" (declare (ignore ?i)) (%collect-used-variables ?e)))))
			       
			       (($if $while) (cons (second (parse-str 'id expr-str)) nil)))))))
	       cfg-map))))


(defmethod kill-initialization ((dfa-kind liveness-analysis))
  (with-accessors ((cfg-map cfg-map) (kill-map kill-map)) dfa-kind
      (maphash (lambda (key value)
		 (setf (gethash key kill-map)
		       (when (> (node-id value) 0)
			 (let ((expr-type (first (node-expr value)))
			       (expr-str (second (node-expr value))))
			   (when (eq expr-type '$stmt)
			     (let ((expr-lst (parse-str 'stmt expr-str)))
			       (forthis expr-lst
				   ((id ?i) (expr ?e))
				 (#! stmt "const ?i := ?e;"
				     (declare (ignore ?e))
				     (second ?i)))))))))
	       cfg-map)))


(defmethod kill-remove ((dfa-kind liveness-analysis) kill join-set)
  (remove kill join-set))


(defmethod join ((dfa-kind liveness-analysis) id)
  (with-accessors ((cfg-map cfg-map) (in-map in-map)) dfa-kind
    (let* ((successors (node-members (gethash id cfg-map)))
	   (out-list (reduce (lambda (acc member-id)
			       (reduce #'cons
				       (gethash member-id in-map)
				       :from-end t
				       :initial-value acc))
			     successors
			     :initial-value nil)))
      (remove-duplicates out-list :test #'equal))))


(defmethod transfer ((dfa-kind liveness-analysis) id join-set)
  (with-accessors ((cfg-map cfg-map) (gen-map gen-map)) dfa-kind
    (let* ((out-after-kill (kill-substracting dfa-kind id join-set))
	   (gen-variables (gethash id gen-map)))
      (mapc (lambda (var)
		  (pushnew var out-after-kill :test #'equal))
	    gen-variables)
      out-after-kill)))


(defmethod point-processing ((dfa-kind liveness-analysis) id)
  (let* ((join-set (unless (= id +global-exit-id+) (join dfa-kind id)))
	 (transfer-set (unless (= id +global-entry-id+) (transfer dfa-kind id join-set))))
    (make-dfa-node
     :in transfer-set
     :out join-set)))
 

(defun compare-and-assign (dfa id new-result)
  "Function that compares new and old result, assigns new result to `dfa' object
   if it's necessary and return predicate of equality (t or nil)."
  (with-accessors ((in-map in-map) (out-map out-map)) dfa
    (let ((predicate (and
		      (equal (dfa-node-in new-result) (gethash id in-map))
		      (equal (dfa-node-out new-result) (gethash id out-map)))))
      (unless predicate
	(setf (gethash id in-map) (dfa-node-in new-result))
	(setf (gethash id out-map) (dfa-node-out new-result)))
      
      predicate)))


(defun traverse (dfa mod-func cfg-map started-id direction-func)
  (let ((visit-trace (make-hash-table)))
    (labels ((%traversal (id)
	       (let ((compare-p (compare-and-assign dfa id (funcall mod-func dfa id))))
		 (when (or (not compare-p) (not (gethash id visit-trace)))
		   (setf (gethash id visit-trace) t)
		   (mapc
		    (lambda (next-id)
		      (%traversal next-id))
		    (funcall direction-func (gethash id cfg-map)))))))
	(%traversal started-id))))


(defun forward-traversal (dfa mod-func)
  (with-accessors ((cfg-map cfg-map)) dfa
    (traverse dfa mod-func cfg-map +global-entry-id+ #'node-members)))


(defun backward-traversal (dfa mod-func)
  (with-accessors ((cfg-map cfg-map)) dfa
    (traverse dfa mod-func cfg-map +global-exit-id+ #'node-parents)))


(defmethod dfa-processing ((dfa-kind liveness-analysis))
  (backward-traversal dfa-kind #'point-processing)
  dfa-kind)
      


;----MAIN DFA FUNCTION----------------------------------------------------------------------
(defun build-dfa (cfg dfa)
  (dfa-initialization dfa cfg)
  (dfa-processing dfa))


(defun test-dfa ()
  (let ((expr '($block
		 ($label "l1")
		 ($stmt "const b := 1;")
		 ($if "g"
		  ($stmt "const a := b + 1;")
		  ($stmt "a + c - 1;"))
		($stmt "const c := b * 2;"))))

    (build-dfa (create-cfg-map expr) (make-instance 'reaching-definitions))
    ;(build-dfa (create-cfg-map expr) (make-instance 'liveness-analysis))
    ))

