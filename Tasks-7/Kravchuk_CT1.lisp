"Control task #1 by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
(defstruct node
   "Structure of tree node."
   value
   children)

(defun create-tree (lst)
   "Function that takes one argument - list `lst',
   creates tree and returns root-node of created tree.
   Root-node is instance of structure `node'."      
   (if (listp lst)
      (make-node
         :value (first lst) 
         :children (mapcar #'create-tree (rest lst)))
      (make-node 
         :value lst)))

;---Task#2-------------------------------------------------------------------------
(defun print-tree (root-node)
   "Function that takes root node `root-node'
   and print tree back into list format."
   (labels ((%temp (node)
      (let ((value (node-value node))
            (children (mapcar #'%temp (node-children node))))
         (if children
            (cons value children)
            value))))
      (format t "~A~%" (%temp root-node))))

;---Task#3--------------------------------------------------------------------------
(defun pre-order-traversal (root-node &key (func #'identity))
   "Function of the Depth-first pre-order traversal.
   Function takes two arguments - root node `root-node'
   and modification function `func' - and returns 
   new modificated tree."
   (make-node
      :value (funcall func (node-value root-node))
      :children (mapcar 
                  (lambda (children) 
                     (pre-order-traversal children :func func))
                  (node-children root-node))))
        
;---Task#4--------------------------------------------------------------------------
(defun post-order-traversal (root-node &key (func #'identity))
   "Function of the Depth-first post-order traversal.
   Function takes two arguments - root node `root-node'
   and modification function `func' - and returns 
   new modificated tree."
   (make-node
      :children (mapcar 
                  (lambda (children) 
                     (post-order-traversal children :func func))
                  (node-children root-node))
      :value (funcall func (node-value root-node))))
   
;---Task#5--------------------------------------------------------------------------
(defun breadth-first-traversal (root-node &key (func #'identity))
   "Function of the Breadth-first traversal.
   Function takes two arguments - root node `root-node'
   and modification function `func' - and returns 
   new modificated tree."
   (let (flag)
      (labels ((%traverse (node-or-lst)
         (cond 
            ((typep node-or-lst 'node)
               (setq flag nil)
               (cons 
                  (funcall func (node-value node-or-lst))
                  (node-children node-or-lst)))
            ((listp node-or-lst)
               (cons 
                  (first node-or-lst)
                  (mapcar #'%traverse (rest node-or-lst))))
		      (t node-or-lst))))
      (create-tree (do ((root-node (%traverse root-node) (%traverse root-node)))
           (flag root-node)
         (setq flag t))))))
          
;---Task#6--------------------------------------------------------------------------
(defun mapTree (root-node &key (traversal #'pre-order-traversal) (func #'identity))
   "Function that takes tree root node `root-node', 
   traversal function `traversal' and modification function `func'.
   Function returns new tree that produced with the applied
   modification function to each node." 
   (funcall traversal root-node :func func))

;---Task#7--------------------------------------------------------------------------
(defun eval-tree (root-node)
   "Function that takes tree root node
   and evaluates this tree by it`s structure."
  (when root-node
      (apply 
         (node-value root-node)
         (mapcar (lambda (elem) 
               (if (node-children elem)
                  (eval-tree elem)
                  (node-value elem))) 
            (node-children root-node)))))

;---TestPack--------------------------------------------------------------------------
(defun test-eval-tree (name root-node expected)
   (let ((result (eval-tree root-node)))
         (or (equal result expected)
         (format t "~%Test ~A failed. Input-tree: ~A~%Result: ~A~%Expected: ~A~%"
            name root-node result expected))))
            
(defun test-mapTree (name root-node traversal-func mod-func expected)
   (let ((result (mapTree root-node :traversal traversal-func :func mod-func)))
         (or (equalp result expected)
         (format t "~%Test ~A failed. Input-tree: ~A~%Result: ~A~%Expected: ~A~%"
            name root-node result expected))))

(defun run-eval-tree-tests ()
   (test-eval-tree "eval-tree-1" (create-tree '(+ 1 2 (* 2 2))) 7)
   (test-eval-tree "eval-tree-2" (create-tree '(+ 1 2 (- 1 2 3) (* 2 3 4))) 23)
   (test-eval-tree "eval-tree-3" (create-tree '(+ 1 2 3 4 5)) 15)
   (test-eval-tree "eval-tree-4" (create-tree '(+ (* 4 2) (* 2 1))) 10))

(defun run-mapTree-tests ()
   (test-mapTree "mapTree-1" (create-tree '(1 (1 1) (1 1)))
                              #'pre-order-traversal #'1+ (create-tree '(2 (2 2) (2 2))))
   (test-mapTree "mapTree-2" (create-tree '(1 (1 1) (1 1)))
                              #'post-order-traversal #'1- (create-tree '(0 (0 0) (0 0))))
   (test-mapTree "mapTree-3" (create-tree '(1 (1 1) (1 1)))
                              #'Breadth-first-traversal #'identity (create-tree '(1 (1 1) (1 1)))))
   
(defun run-task-tests ()
   (run-mapTree-tests)
   (run-eval-tree-tests))
 
(run-task-tests)  