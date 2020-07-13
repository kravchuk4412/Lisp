(uiop:define-package :hw9/cfg/impl
    (:use :cl
          :cl-graph)
  (:export #:cfg-traversal
	   #:create-cfg-map
	   #:cfg-to-dot
	   #:cfg-to-cl-graph
	   #:node
	   #:$block
	   #:$stmt
	   #:$if
	   #:$label
	   #:$while
	   #:$goto))

(in-package :hw9/cfg/impl)

;---Structure of cfg-map node---------------------------
(defstruct node
  "`id' - number, that is identifier of node. His range - from -1 to infinity.
   Only special nodes like 'ENTRY' or 'EXIT' have `id' equal 0 and -1 respectively.
   `expr' - list of format (<type of statement> <string of code>).
   Keeps line of code, that is assigned to the current node.
   `parents' - list of predecessors of current node. Predecessors are encrypted by their `id'.
   `members' - list of successors of current node. Successors are encrypted by their `id'."
  
  id
  expr
  parents
  members)


;---Structure of dfa-map node---------------------------
(defstruct dfa-node
  "`in' - set of facts at entry of node.
   `out' - set of facts at exit of node."

  in
  out)

;---Creation of cfg-likes tree---------------------------
(defun create-cfg-map (cfg-expr)
  "Function that receives cfg-expression `cfg-expr' and returns it in the hash-table format.
   Numbers are keys, `node'-s are values. To each key corresponds a single node.
   Every key is the same as `node-id' of current `node' in value."
  (let ((label-map (make-hash-table :test #'equal))
	(goto-map (make-hash-table :test #'equal))
	(nodes-map (make-hash-table))
	(node-number 1))
    
    (labels ((%adopted (child-id parent-id)
	       (let ((parent (gethash parent-id nodes-map)))
		 (setf (node-members parent) (append (node-members parent) (list child-id)))))

	     (%adopt-child-to-all-parents (child-id parents-id)
	       (mapc (lambda (elem) (%adopted child-id elem)) parents-id))

	     (%add-to-nodes-map (node &optional number)
	       (if number
		   (setf (gethash number nodes-map) node)
		   (prog2
		       (setf (gethash node-number nodes-map) node)
		       (incf node-number))))
	     
	     (%add-parents (node-id parents-id)
	       (setf (node-parents (gethash node-id nodes-map))
		  (append (node-parents (gethash node-id nodes-map)) parents-id)))

	     (%recursive-traversal (expr parent)
	       (let* ((type (first expr)))
		 (case type
		   ($block (reduce
			    (lambda (acc elem)
				(%recursive-traversal elem acc))
			    (cdr expr)
			    :initial-value parent))
		   ($stmt (let ((node (make-node :id node-number
						 :expr expr
						 :parents parent)))
			    (%adopt-child-to-all-parents (node-id node) parent)
			    (%add-to-nodes-map node)
			    (list (node-id node))))
		   ($label (let ((node (make-node :id node-number
						  :expr expr
						  :parents parent)))
			     (%adopt-child-to-all-parents (node-id node) parent)
			     (setf (gethash (second expr) label-map) (node-id node))
			     (when (gethash (second expr) goto-map)
			       (%adopt-child-to-all-parents (node-id node) (gethash (second expr) goto-map))
			       (setf (node-parents node) (append (node-parents node)(gethash (second expr) goto-map)))
			       (remhash (second expr) goto-map))
			     (%add-to-nodes-map node)
			     (list (node-id node))))
		   ($if (let ((node (make-node :id node-number
					       :expr (list (first expr) (second expr))
					       :parents parent)))
			  (%adopt-child-to-all-parents (node-id node) parent)
			  (%add-to-nodes-map node)
			  (let ((parent (list (node-id node))))
			    (append
			     (%recursive-traversal (third expr) parent)
			     (%recursive-traversal (fourth expr) parent)))))
		   ($while (let ((node (make-node :id node-number
						  :expr (list (first expr) (second expr))
						  :parents parent)))
			     (%adopt-child-to-all-parents (node-id node) parent)
			     (%add-to-nodes-map node)
			     (let* ((parent (list (node-id node)))
				    (last-operator-in-while (%recursive-traversal (third expr) parent)))
			       (%adopt-child-to-all-parents (node-id node) last-operator-in-while)
			       (setf (node-parents (gethash (node-id node) nodes-map))
				     (append (node-parents (gethash (node-id node) nodes-map)) last-operator-in-while))
			       (list (node-id node)))))
		   ($goto (let ((node (make-node :id node-number
						 :expr expr
						 :parents parent)))
			    (%adopt-child-to-all-parents (node-id node) parent)
			    (%add-to-nodes-map node)
			    (if (gethash (second expr) label-map)
				(let ((label-node (gethash (gethash (second expr) label-map) nodes-map)))
				  (%adopted (gethash (second expr) label-map) (node-id node))
				  (setf (node-parents label-node) (append (node-parents label-node) (list (node-id node)))))
				(push (node-id node) (gethash (second expr) goto-map)))
			    (list (node-id node))))))))
      (when cfg-expr
	(let* ((start-node (make-node :expr "ENTRY" :id 0))
	       (end-node (make-node :expr "EXIT" :id -1))
	       (parent (list (node-id start-node))))
	  
	  (%add-to-nodes-map start-node 0)
	  (%add-to-nodes-map end-node -1)
	  (let ((last-operator (%recursive-traversal cfg-expr parent)))
	    (%adopt-child-to-all-parents (node-id end-node) last-operator)
	    (%add-parents (node-id end-node) last-operator))
	  nodes-map)))))

;---Pretty print--------------------------------------------------------------------
(defun cfg-map-pretty-print (cfg-map)
  (maphash #'(lambda (key value)
	       (format t "~%KEY: ~A~%VALUE: ~A" key (node-expr value))
	       (mapc #'(lambda (id)
			 (format t "~%CHILD: ~A" (node-expr (gethash id cfg-map))))
		     (node-members value))
	       (mapc #'(lambda (id)
			 (format t "~%PARENT: ~A" (node-expr (gethash id cfg-map))))
		     (node-parents value))
	       (format t "~%"))
	   cfg-map))


;---Cfg traversal-----------------------------------------------
(defun map-identity (node) node)

(defun cfg-traversal (cfg-map &key  (func #'map-identity))
  "Function receives `cfg-map' and function `func' which modify every node of this map.
   Current node will be replaced be return of the `func'.
   Function returns a new modified map."
  (let ((visit-trace (make-hash-table))
	(new-cfg-map (make-hash-table)))
    (labels ((%copy-node (id)
	       (let ((node (gethash id cfg-map)))
		 (make-node
		  :id (node-id node)
		  :expr (node-expr node)
		  :parents (node-parents node)
		  :members (node-members node))))
	     
	     (%next-node (id)
	       (unless (gethash id visit-trace)
		 (setf (gethash id visit-trace) t)
		 (setf (gethash id new-cfg-map)
		       (funcall func (%copy-node id)))
		 (mapc
		  (lambda (next-id)
		    (%next-node next-id))
		  (node-members (gethash id cfg-map))))))
      (%next-node 0)
      new-cfg-map)))


;---Reaching definition------------------------------------
(defun reaching-definitions (cfg-map)
  (let ((gen-map (make-hash-table))
	(kill-map (make-hash-table))
	(dfa-map (make-hash-table)))
    
    (labels ((%dfa-map-initialization ()
	       (maphash #'(lambda (key value)
			    (declare (ignore value))
			    (setf (gethash key dfa-map) (make-dfa-node)))
			cfg-map))
	     )
      
      (%dfa-map-initialization)
      )
    )
  )

;TEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEESTTTTTTTTTTTTTTTTTTTTTT
(defun TEST-MAP ()
    (let* ((test-expr '($block
		     ($label "l1")
		     ($stmt "const b := a + 1;")
		     ($while "a"
		      ($block
		       ($stmt "a = a + 1;")
		       ($stmt "a = a - 1;")))
		     ($stmt "const a := b * 2;")
		     ($if "a"
		      ($goto "l1")
		      ($stmt "c;"))
			($stmt "a = a * 3;")))
	   (table (create-cfg-map test-expr)))
      (reaching-definitions table)
      ;;(cfg-map-pretty-print table)
      ;;(cfg-map-pretty-print (cfg-traversal table))
      ))


;---Dump cfg to cl-graph-----------------------------------
(defun cfg-to-cl-graph (cfg-map)
  "Function that receives cfg graph in map-form and
   returns cfg-graph in `cl-graph' form."
  (let ((graph (make-graph 'graph-container :default-edge-type :directed))
	(first-visit-trace (make-hash-table)))
    (labels ((%recursive-create (cur-id cur-vertex)
	       (unless (gethash cur-id first-visit-trace)
		 (setf (gethash cur-id first-visit-trace) t)
		 (mapc (lambda (id)
			 (let* ((node (gethash id cfg-map)) 
				(new-vertex (add-vertex graph (node-expr node))))
			   (add-edge-between-vertexes graph cur-vertex new-vertex)
			   (%recursive-create id new-vertex)))
			 (node-members (gethash cur-id cfg-map))))))
      (%recursive-create 0 (add-vertex graph (node-expr (gethash 0 cfg-map))))
      graph)))


;---Dump cfg to dot--------------------
(defun cfg-to-dot (cl-graph-cfg &key file-path)
  "Function that receives cfg-graph in `cl-graph' form
   and dumps it in dot form. Returns either string 
   or output it in file, that is situated by `file-path'."
  (graph->dot cl-graph-cfg file-path :vertex-labeler #'princ))

