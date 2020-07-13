;---Task#1--------------------------------------------------------------------------
(defclass state-machine ()
  ((initial-state
    :accessor initial-state
    :initform :start
    :allocation :class)
   (errors
    :accessor errors
    :initform nil)))

(defgeneric start-work (machine)
   (:documentation "Generic function that runs state machine."))
   
(defgeneric lifecycle (machine state)
   (:documentation "Generic function that implements lifecycle of state machine."))

(defgeneric finish-work (machine)
   (:documentation "Generic function that finishes work of state-machine."))

;---Task#3--------------------------------------------------------------------------
(defclass coffee-state-machine (state-machine)
   ((result
    :accessor result
    :initform nil)))

(defun prompt-read (prompt)
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
            
(defun read-coffee-type ()
   (let ((answer (prompt-read 
            "Please type the type of coffee (cappuccino\\latte\\espresso) or anything else to quit the application")))
      (when (or (equal answer "cappuccino")
                (equal answer "latte")
                (equal answer "espresso"))
            answer)))
            
(defun read-coffee-size ()
   (let ((answer (prompt-read 
            "Please type the size of the drink (small\\medium\\large) or anything else to quit the application")))
      (when (or (equal answer "small")
                (equal answer "medium")
                (equal answer "large"))
            answer)))

(defun read-amount-of-sugar ()
   (let ((answer (prompt-read 
            "Please type number of sugar cubes (0\\1\\2) or anything else to quit the application")))
      (when (or (equal answer "0")
                (equal answer "1")
                (equal answer "2"))
            answer)))
  
(defmethod lifecycle ((machine coffee-state-machine) state)
   (with-accessors ((errors errors)
                    (result result)
                    (initial-state initial-state))
         machine
      (cond
         ((equal state :start) 
            (prog2 
               (format t "~%Welcome to a virtual coffee machine!~%")
               (lifecycle machine :coffee-type)))
         ((equal state :coffee-type)
            (let ((res (read-coffee-type)))
               (if res
                  (prog2 
                     (setf result (cons res result))
                     (lifecycle machine :coffee-size))
                  (lifecycle machine :error))))
         ((equal state :coffee-size)
            (let ((res (read-coffee-size)))
               (if res
                  (prog2 
                     (setf result (cons res result))
                     (lifecycle machine :sugar))
                  (lifecycle machine :error))))
         ((equal state :sugar)
            (let ((res (read-amount-of-sugar)))
               (if res
                  (prog2 
                     (setf result (cons res result))
                     (lifecycle machine :finish))
                  (lifecycle machine :error))))
         ((equal state :error)
            (setf errors "The end! You close the program or an incorrect value was entered during the program."))
         ((equal state :finish)
            (format t "Wait a second, preparing coffee...~%")))))
 
(defmethod start-work ((machine coffee-state-machine))
   (when (result machine)
      (setf (result machine) nil))
   (when (errors machine)
      (setf (errors machine) nil))
   (lifecycle machine (initial-state machine))
   (finish-work machine))
   
(defmethod finish-work ((machine coffee-state-machine))
   (with-accessors ((errors errors)
                    (result result))
         machine
      (if errors
         (format t "~%~A" errors)
         (format t "Your ~A ~A with ~A cubes of sugar is ready. Bon appetit!"
            (nth 1 result) (nth 0 result) (nth 2 result)))))
   
(defvar csm (make-instance 'coffee-state-machine))

(start-work csm)
   

