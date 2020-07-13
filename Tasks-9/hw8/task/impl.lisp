(uiop:define-package :hw8/task/impl
    (:use :cl
          :hw8/lexer-parser/all
	  :hw8/macro-characters/all
          :hw8/cfg/all
	  :hw8/dfa/all)
  (:export))

(in-package :hw8/task/impl)

(defun create-dead-code-map (cfg-map)
  (let ((dead-code-map (make-hash-table))) ;Keep `t', if node alive 
    (labels ((%go-to-p (id)
	       (when (> id 0)
		 (eq '$goto (first (node-expr (gethash id cfg-map))))))
	     
	     (%label-p (id label)
	       (let ((expr (node-expr (gethash id cfg-map))))
		 (unless (eql id +global-exit-id+)
		   (and (eq '$label (first expr)) 
			(equal label (second expr))))))

	     (%determine-alive-members (id)
	       (reduce (lambda (member-id acc)
			 (if (or (eql member-id +global-exit-id+)
				 (%label-p member-id (second (node-expr (gethash id cfg-map)))))
			     (cons member-id acc)
			     acc)) 
			 (node-members (gethash id cfg-map))
			 :initial-value nil
			 :from-end t))
	     
	     (%cfg-traversal (id)
	       (unless (gethash id dead-code-map)
		 (setf (gethash id dead-code-map) t)
		 
		 (when (%go-to-p id)
		   (setf (node-members (gethash id cfg-map))
			 (%determine-alive-members id)))
		 (mapc
		  (lambda (next-id)
		    (%cfg-traversal next-id))
		  (node-members (gethash id cfg-map))))))
      (%cfg-traversal +global-entry-id+)
      dead-code-map)))

;Task 3.1
(defun delete-cfg-dead-code (cfg-map)
  (let ((dead-code-map (create-dead-code-map cfg-map)))
    (labels ((%delete-from-parents (key-id id-list)
	       (mapc (lambda (parent-id)
		       (unless (gethash parent-id dead-code-map)
			 (setf (node-members (gethash parent-id cfg-map))
			       (remove key-id (node-members (gethash parent-id cfg-map))))))
		     id-list))
	     
	     (%delete-from-members (key-id id-list)
	       (mapc (lambda (member-id)
		       (unless (gethash member-id dead-code-map)
			 (setf (node-parents (gethash member-id cfg-map))
			       (remove key-id (node-parents (gethash member-id cfg-map))))))
		     id-list)))

      (maphash (lambda (key value)
		 (unless (gethash key dead-code-map)
		   (%delete-from-parents key (node-parents value))
		   (%delete-from-members key (node-members value))
		   (remhash key cfg-map)))
	       cfg-map)
      
      cfg-map)))


;Task 3.2
(defun if-processing (id cfg-map)
  (let ((if-node-members (node-members (gethash id cfg-map))))
    (labels ((%search-common-node-inner (true-branch-id false-branch-id)
	       (let ((find-result (find true-branch-id (node-members false-branch-id))))
		 (if find-result
		     find-result
		     (reduce (lambda (acc member-id)
			       (declare (ignore acc))
			       (%search-common-node-inner true-branch-id member-id))
			     (node-members (gethash false-branch-id cfg-map))))))
	     
	     (%search-common-node (true-branch-id false-branch-id)
	       (let ((search-result (reduce (lambda (acc member-id)
					      (declare (ignore acc))
					    (%search-common-node-inner member-id false-branch-id))
					    (node-members (gethash true-branch-id cfg-map)))))
		 (if search-result
		     search-result
		     (mapc (lambda (member-id)
			     (%search-common-node member-id false-branch-id))
			   (node-members (gethash true-branch-id cfg-map))))))

	     (%delete-full-if ())

	     (%delete-from-id1-to-id2 ()))

      (let ((common-node (%search-common-node (first if-node-members) (second if-node-members))))
	(if (eql common-node (second if-node-members))
	    (%delete-full-if)
	    (%delete-from-id1-to-id2)
      )))))

(defun while-processing (id cfg-map)
  (declare (ignore id))
  (declare (ignore cfg-map)))

(defun delete-rd-dead-code (cfg-map)
  (let ((dfa (build-dfa cfg-map (make-instance 'reaching-definitions))))
    (with-accessors ((cfg-map cfg-map) (out-map out-map)) dfa
      (let ((visit-trace (make-hash-table))) 
	(labels ((%variable-usage-p (id line)
		   (let ((variable (second (parse-str 'id line))))
		     (find variable (gethash id out-map) :key #'car)))
		 
		 (%switch-oper-p (id)
		   (let* ((expr (node-expr (gethash id cfg-map)))
			  (type (first expr))
			  (line (second expr)))
		     (when (or (eq type '$if) (eq type '$while))
		       (unless (%variable-usage-p id line)
			 (if (eq type '$if)
			     (if-processing id cfg-map)
			     (while-processing id cfg-map))))))			    
		 
		 (%cfg-traversal (id)
		   (unless (gethash id visit-trace)
		     (setf (gethash id visit-trace) t)

		     (when (> id 0)
		       (%switch-oper-p id))
		     
		     (mapc
		      (lambda (next-id)
			(%cfg-traversal next-id))
		      (node-members (gethash id cfg-map))))))
	  (%cfg-traversal +global-entry-id+)
	  cfg-map)))))



;Task 3.3
#|(defun delete-liveness-dead-code (cfg-map)
  (let ((dfa (build-dfa cfg-map (make-instance 'liveness-analysis))))
    (with-accessors ((cfg-map cfg-map) (out-map out-map)) dfa
      (let ((visit-trace (make-hash-table))) 
	(labels ((%definition-p (line)
		   (forthis line
		       ((id ?i) (expr ?e))
		     (#! stmt "const ?i := ?e;"
			 (declare (ignore ?e)) (second ?i))))

		 (%stmt-p (id)
		   ()))
		 
		 (%cfg-traversal (id)
		   (unless (gethash id visit-trace)
		     (setf (gethash id visit-trace) t)

		     (when (> id 0)
		       (when (%definition-p id)
			 ( id))
		     
		     (mapc
		      (lambda (next-id)
			(%cfg-traversal next-id))
		      (node-members (gethash id cfg-map))))))
	  (%cfg-traversal +global-entry-id+)
	  cfg-map)))))
|#

    
(defun test-task ()
  (let* ((case1 '($block
		  ($label "l1")
		  ($if "g"
		   ($stmt "const a := b + 1;")
		   ($stmt "a + c - 1;"))
		  ($stmt "const c := b * 2;")
		  ($goto "l1")))

	 (case2 '($block
		  ($goto "l1")
		  ($if "g"
		   ($stmt "const a := b + 1;")
		   ($stmt "a + c - 1;"))
		  ($stmt "const c := b * 4;")
		  ($stmt "const c := b * 5;")
		  ($label "l2")
		  ($stmt "const c := b * 2;")
		  ($stmt "const c := b * 3;")
		  ($label "l1")
		  ($goto "l2")))
	 
	 (case3 '($block
		  ($goto "l1")
		  ($if "g"
		   ($stmt "const a := b + 1;")
		   ($stmt "a + c - 1;"))
		  ($label "l2")
		  ($stmt "const c := b * 2;")
		  ($stmt "const c := b * 2;")
		  ($goto "l2")
		  ($lable "l1")))

	 (cfg (delete-cfg-dead-code (create-cfg-map case2))))
    
    (declare (ignorable case1))
    (declare (ignorable case2))
    (declare (ignorable case3))
    
    ;(cfg-to-dot-my-path (cfg-to-cl-graph cfg))
    cfg))
