"Lecture #4 task by Oleksandr Kravchuk"

;---Task#1--------------------------------------------------------------------------
;Implement state-machine API using CLOS

;---Class definitions------------------------------------------------
(defclass state ()
   ((name
    :accessor name
    :initarg :name
    :initform nil)))   

(defclass state-machine ()
  ((initial-state
    :accessor initial-state
    :initarg initial-state
    :initform nil)
   (current-state
    :accessor current-state
    :initform nil)
   (errors
    :accessor errors
    :initform nil)))

;---Generic functions------------------------------------------------    
(defgeneric start-work (machine)
   (:documentation "Generic function that runs state machine."))
   
(defgeneric lifecycle (machine)
   (:documentation "Generic function that implements lifecycle of state machine."))

(defgeneric state-input (state)
   (:documentation "Generic function that returns processed input for the appropriate state."))

(defgeneric perform-state (state input)
   (:documentation "Generic function that calls state method and returns next state."))

(defgeneric finish-work (machine)
   (:documentation "Generic function that finishes work of state-machine."))

;---General methods---------------------------------------------------  
(defmethod start-work ((machine state-machine))
   (setf (current-state machine) initial-state)
   (setf (errors machine) nil)
   (lifecycle machine)
   (finish-work))

(defmethod lifecycle ((machine state-machine))
   (with-accessors ((current-state current-state))
         machine
      (when current-state
         (let* ((input (state-input current-state))
                (next-state (perform-state current-state input)))
            (setf current-state next-state)
            (lifecycle machine)))))

;---Task#3--------------------------------------------------------------------------
;Run coffee state-machine using this API:
;(defvar csm (make-instance 'coffee-state-machine))
;(start-work csm)

;---Class definition------------------------------------------------
(defclass coffee-state-machine (state-machine)
   ((result
    :accessor result
    :initform nil
    :documentation "Lifecycle use this variable for saving result." )))

#|
;---Read text from console-------------------------------------------
(defun prompt-read (prompt)
   "Function that takes string `prompt', prints it
   and reads input from concole."
  (format *query-io* "~A: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;---States functions------------------------------------------------- 
(defun read-coffee-type ()
   "Function that return type of cofee or NIL."
   (let ((answer (prompt-read 
            "Please type the type of coffee (cappuccino\\latte\\espresso) or anything else to quit the application")))
      (when (or (equal answer "cappuccino")
                (equal answer "latte")
                (equal answer "espresso"))
            answer)))
            
(defun read-coffee-size ()
   "Function that return size of drink or NIL."
   (let ((answer (prompt-read 
            "Please type the size of the drink (small\\medium\\large) or anything else to quit the application")))
      (when (or (equal answer "small")
                (equal answer "medium")
                (equal answer "large"))
            answer)))
            
(defun read-amount-of-sugar ()
   "Function that return number of sugar cubes or NIL."
   (let ((answer (prompt-read 
            "Please type number of sugar cubes (0\\1\\2) or anything else to quit the application")))
      (when (or (equal answer "0")
                (equal answer "1")
                (equal answer "2"))
            answer)))
  
;---State-machine defmethods for coffee-machine-------------------------- 
(defmethod lifecycle ((machine coffee-state-machine))
   "Lifecicle of coffee-state-machine."
   (with-accessors ((errors errors)
                    (result result)
                    (initial-state initial-state))
         machine
      (labels ((%cycle (state)
                  (cond
                     ((equal state :start) 
                        (prog2 
                           (format t "~%Welcome to a virtual coffee machine!~%")
                           (%cycle :coffee-type)))
                     ((equal state :coffee-type)
                        (let ((res (read-coffee-type)))
                           (if res
                              (prog2 
                                 (setf result (cons res result))
                                 (%cycle :coffee-size))
                              (%cycle :error))))
                     ((equal state :coffee-size)
                        (let ((res (read-coffee-size)))
                           (if res
                              (prog2 
                                 (setf result (cons res result))
                                 (%cycle :sugar))
                              (%cycle :error))))
                     ((equal state :sugar)
                        (let ((res (read-amount-of-sugar)))
                           (if res
                              (prog2 
                                 (setf result (cons res result))
                                 (%cycle :finish))
                              (%cycle :error))))
                     ((equal state :error)
                        (setf errors "The end! You close the program or an incorrect value was entered during the program."))
                     ((equal state :finish)
                        (format t "~%Wait a second, preparing coffee...~%")))))
         (%cycle initial-state)
         (setf result (reverse result)))))
 
(defmethod start-work ((machine coffee-state-machine))
   "Function thar start state-machine with initial-state,
   resets class variables and invokes `finish-work' function
   for processing result."
   (setf (result machine) nil)
   (setf (errors machine) nil)
   (lifecycle machine)
   (finish-work machine))
   
(defmethod finish-work ((machine coffee-state-machine))
   "Function that handles the result after lifecycle."
   (with-accessors ((errors errors)
                    (result result))
         machine
      (if errors
         (format t "~%~A" errors)
         (format t "~%Your ~A ~A with ~A cubes of sugar is ready. Bon appetit!"
            (nth 1 result) (nth 0 result) (nth 2 result)))))
|#   
;---Task#4--------------------------------------------------------------------------
;Create ATM state-machine using this API
;(defvar atm (make-instance 'atm-state-machine :balance 1000))
;(start-work atm)

;---Class definition------------------------------------------------
(defclass atm-state-machine (state-machine)
   ((balance
    :accessor balance
    :initarg :balance
    :initform 0)))

;---States functions------------------------------------------------- 
(defun enter-pin-code ()
   "Function that checks PIN-code and returns next state."
   (let ((answer (prompt-read 
            "Please, enter PIN-code (1111) or anything else to quit the application")))
      (if (equal answer "1111")
            :operations-menu
            :error)))
            
(defun choose-operation ()
   "Function that allows choose operation and returns next state."
   (let ((answer (prompt-read 
            "Please, exactly type next operation (or anything else to quit the application):
               -> print balance;
               -> withdraw money;
               -> replenish balance;
            Your choice")))
      (cond 
         ((equal answer "print balance") :print-balance)
         ((equal answer "withdraw money") :withdraw-money)
         ((equal answer "replenish balance") :replenish-balance)
         (t :error))))

(defun print-balance (balance)
   "Function that print balance and returns next state."
   (format t "~%Your balance: ~A c.o.~%" balance)
   :check-continue)
         
(defun check-continue ()
   "Function that checks - continue of work or not - and returns next state."
   (let ((answer (prompt-read 
            "Continue to work? (y\\n) or anything else to quit the application")))
      (cond 
         ((equal answer "y") :operations-menu)
         ((equal answer "n") :finish)
         (t :error))))
         
(defun withdraw-money (machine)
   "Function that allows withdraw money from balance and returns next state."
   (let* ((answer (prompt-read 
            "How much do you want to withdraw? Enter number"))
            (withdraw (parse-integer answer)))
      (cond 
         ((<= withdraw 0) :error) 
         ((< (- (balance machine) withdraw) 0)
                                       (format t "The balance on your account is not enough.~%")
                                       :check-continue)
         (t
            (decf (balance machine) withdraw)
            (format t "Success!~%")
            :check-continue))))
 
(defun replenish-balance (machine)
   "Function that allows replenish balance and returns next state."
   (let* ((answer (prompt-read 
            "How much do you want to replenish? Enter number"))
            (replenish (parse-integer answer)))
      (if (<= replenish 0)
         :error 
         (progn 
            (format t "Success!~%")
            (incf (balance machine) replenish)
            :check-continue))))

;---State-machine defmethods for ATM-------------------------------            
(defmethod lifecycle ((machine atm-state-machine))
   "Lifecicle of ATM."
   (with-accessors ((errors errors)
                    (balance balance)
                    (initial-state initial-state))
         machine
      (labels ((%cycle (state)
                  (cond
                     ((equal state :start)          
                        (format t "~%Welcome to an ATM!~%")
                        (%cycle :pin-code))
                     ((equal state :pin-code)
                        (%cycle (enter-pin-code)))
                     ((equal state :operations-menu)
                        (%cycle (choose-operation)))
                     ((equal state :print-balance)
                        (%cycle (print-balance balance)))
                     ((equal state :check-continue)
                        (%cycle (check-continue)))
                     ((equal state :withdraw-money)
                        (%cycle (withdraw-money machine)))
                     ((equal state :replenish-balance)
                        (%cycle (replenish-balance machine)))
                     ((equal state :error)
                        (setf errors "The end! You close the program or an incorrect value was entered during the program."))
                     ((equal state :finish)
                        (format t "~%Thank you for trusting us!~%")))))
         (%cycle initial-state))))
         

(defmethod start-work ((machine atm-state-machine))
   "Function thar start state-machine with initial-state,
   resets errors variablse and invokes `finish-work' function
   for handles errors."
   (setf (errors machine) nil)
   (lifecycle machine)
   (finish-work machine))
   
(defmethod finish-work ((machine atm-state-machine))
   "Function that handles the errors after lifecycle."
   (with-accessors ((errors errors)
                    (balance balance))
         machine
      (if errors
         (format t "~%~A" errors)
         (format t "~%Your final balance: ~A c.o.~%" balance))))
