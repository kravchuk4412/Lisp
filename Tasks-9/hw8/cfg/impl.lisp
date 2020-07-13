(uiop:define-package :hw8/cfg/impl
    (:use :cl
          :cl-graph
	  :hw8/lexer-parser/all
	  :hw8/macro-characters/all
	  :hw8/utils)
  (:export #:cfg-traversal
	   #:create-cfg-map
	   #:cfg-to-dot
	   #:cfg-to-cl-graph
	   #:node
	   #:make-node
	   #:node-id
	   #:node-parents
	   #:node-members
	   #:node-expr
	   #:cfg-map-pretty-print 
	   #:$block
	   #:$stmt
	   #:$if
	   #:$label
	   #:$while
	   #:$goto
	   #:+global-entry-id+
	   #:+global-exit-id+
	   #:cfg-to-dot-my-path))

(in-package :hw8/cfg/impl)

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

(defconstant +global-entry-id+ 0 "Id of the start-node.")

(defconstant +global-exit-id+ -1 "Id of the exit-node.")

;---Creation of cfg-map---------------------------
(defun create-cfg-map (cfg-expr)
  "Function that receives cfg-expression `cfg-expr' and returns it in the hash-table format.
   Numbers are keys, `node'-s are values. To each key corresponds a single node.
   Every key is the same as `node-id' of current `node' in value."
  (let ((label-map (make-hash-table :test #'equal))
	(goto-map (make-hash-table :test #'equal))
	(nodes-map (make-hash-table))
	(node-number 0))
    
    (labels ((%mk-node (expr parents &optional id)
	       (make-node :id (if id id (incf node-number))
			  :expr expr
			  :parents parents))
      
	     (%adopted (child-id parent-id)
	       (let ((parent (gethash parent-id nodes-map)))
		 (setf (node-members parent) (append (node-members parent) (list child-id)))))

	     (%adopt-child-to-all-parents (child-id parents-id)
	       (mapc (lambda (elem) (%adopted child-id elem)) parents-id))

	     (%add-to-nodes-map (node &optional number)
	       (setf (gethash (if number number node-number) nodes-map) node))	  
	     
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
		   ($stmt (let ((node (%mk-node expr parent)))
			    (%adopt-child-to-all-parents (node-id node) parent)
			    (%add-to-nodes-map node)
			    (list (node-id node))))
		   ($label (let ((node (%mk-node expr parent)))
			     (%adopt-child-to-all-parents (node-id node) parent)
			     (setf (gethash (second expr) label-map) (node-id node))
			     (when (gethash (second expr) goto-map)
			       (%adopt-child-to-all-parents (node-id node) (gethash (second expr) goto-map))
			       (setf (node-parents node) (append (node-parents node) (gethash (second expr) goto-map)))
			       (remhash (second expr) goto-map))
			     (%add-to-nodes-map node)
			     (list (node-id node))))
		   ($if (let ((node (%mk-node (list (first expr) (second expr)) parent)))
			  (%adopt-child-to-all-parents (node-id node) parent)
			  (%add-to-nodes-map node)
			  (let ((parent (list (node-id node))))
			    (append
			     (%recursive-traversal (third expr) parent)
			     (if (fourth expr)
				 (%recursive-traversal (fourth expr) parent)
				 parent)))))
		   ($while (let ((node (%mk-node (list (first expr) (second expr)) parent)))
			     (%adopt-child-to-all-parents (node-id node) parent)
			     (%add-to-nodes-map node)
			     (let* ((parent (list (node-id node)))
				    (last-operator-in-while (%recursive-traversal (third expr) parent)))
			       (%adopt-child-to-all-parents (node-id node) last-operator-in-while)
			       (setf (node-parents (gethash (node-id node) nodes-map))
				     (append (node-parents (gethash (node-id node) nodes-map)) last-operator-in-while))
			       (list (node-id node)))))
		   ($goto (let ((node (%mk-node expr parent)))
			    (%adopt-child-to-all-parents (node-id node) parent)
			    (%add-to-nodes-map node)
			    (if (gethash (second expr) label-map)
				(let ((label-node (gethash (gethash (second expr) label-map) nodes-map)))
				  (%adopted (gethash (second expr) label-map) (node-id node))
				  (setf (node-parents label-node) (append (node-parents label-node) (list (node-id node)))))
				(push (node-id node) (gethash (second expr) goto-map)))
			    (list (node-id node))))))))
      (when cfg-expr
	(let* ((start-node (%mk-node "ENTRY" nil +global-entry-id+))
	       (end-node (%mk-node "EXIT" nil +global-exit-id+))
	       (parent (list (node-id start-node))))
	  
	  (%add-to-nodes-map start-node +global-entry-id+)
	  (%add-to-nodes-map end-node +global-exit-id+)
	  (let ((last-operator (%recursive-traversal cfg-expr parent)))
	    (%adopt-child-to-all-parents (node-id end-node) last-operator)
	    (%add-parents (node-id end-node) last-operator))
	  nodes-map)))))


;---Pretty print--------------------------------------------------------------------
(defun cfg-map-pretty-print (cfg-map)
   "Function that prints all nodes in map
   with their parents and children."
  (maphash (lambda (key value)
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
      (%next-node +global-entry-id+)
      new-cfg-map)))


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
      (%recursive-create +global-entry-id+
			 (add-vertex graph (node-expr (gethash +global-entry-id+ cfg-map))))
      graph)))


;---Dump cfg to dot--------------------
(defun cfg-to-dot (cl-graph-cfg &key file-path)
  "Function that receives cfg-graph in `cl-graph' form
   and dumps it in dot form. Returns either string 
   or output it in file, that is defined by `file-path'."
  (graph->dot cl-graph-cfg file-path :vertex-labeler #'princ))


(defun cfg-to-dot-my-path (cfg)
    (cfg-to-dot cfg :file-path "D:\\Lisp\\hw8\\cfg.dot"))
