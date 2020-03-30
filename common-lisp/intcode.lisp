(in-package :cl-user)
(defpackage :aoc19-intcode
  (:use :cl
        :aoc19-utils
        :cl-interpol
        :trivial-arguments)
  (:nicknames aoc-intcode)
  (:export
   #:compile-program
   #:compute
   #:register-op
   #:read-intcode-program
   #:mem/r
   #:mem/w))
(in-package :aoc19-intcode)

;(cl-interpol:enable-interpol-syntax)

(defun read-intcode-program (filepath)
  "Reads the program from disk as a list of integers"
  (flet ((split-to-integers (s)
           (mapcar #'parse-integer (uiop:split-string s :separator ","))))
    (split-to-integers (uiop:read-file-string filepath))))


(defparameter *ops* (make-hash-table)
  "Stores the possible ops")

(defstruct op
  "output-arg will serve to override program-mode -> immediate mode for any   write argument."
  opcode
  n-args
  fn
  output-arg
  op-name)

(defun get-output-arg (fn)
 "Retrieves the output argument position."
 (position '$OUT (arg:arglist fn)))

(defun register-op (opcode op-name fn)
  (setf (gethash opcode *ops*)
        (make-op :opcode opcode
                 :n-args (aoc19-utils:get-function-mandatory-arguments-count fn)
                 :fn fn
                 :output-arg (get-output-arg fn)
                 :op-name op-name)))

(defun mem/r (memory address)
  (elt memory address))

(defun mem/w (memory address value)
  "Writes the value to memory at address. Returns nil."
  (setf (elt memory address) value)
  nil)

(register-op 1 "+"
             (lambda (a b $out &key mem)
               (mem/w mem $out (+ a b))))

(register-op 2 "*"
             (lambda (a b $out &key mem)
               (mem/w mem $out (* a b))))

(register-op 3 "input"
             (lambda ($out &key mem)
               (print mem)
               (format *query-io* "Program input:")
               (mem/w mem $out (parse-integer (read-line)))
               (finish-output *query-io*)
               nil))

(register-op 4 "output"
             (lambda (a &key mem)
               (declare (ignore mem))
               a))

(register-op 5 "jump-if-true"
             (lambda (a b &key mem)
               (declare (ignore mem))
               (when (/= 0 a)
                 (list :JUMP b))))

(register-op 6 "jump-if-false"
             (lambda (a b &key mem)
               (declare (ignore mem))
               (when (= 0 a)
                 (list :JUMP b))))

(register-op 7 "<"
             (lambda (a b $out &key mem)
               (mem/w mem $out
                      (if (< a b) 1 0))))

(register-op 8 "="
             (lambda (a b $out &key mem)
               (mem/w mem $out
                      (if (= a b) 1 0))))

(register-op 99 "exit"
             (lambda (&key mem)
               (declare (ignore mem))
               :EXIT))

(defun fetch-op (opcode)
  (let ((op (gethash opcode *ops*)))
    (if (null op)
        (error (format nil "Invalid/Unknown Opcode (~A)!" opcode))
        op)))

(defun instruction-opcode (instruction)
  (mod instruction 100))

(defun length-integer (n)
  "Log-base-10 + ceiling of an integer + fractional part gives the #digits."
  (ceiling (log (+ n 0.1) 10)))

(defun instruction-modes (instruction len)
  "Parses the instruction modes generating flags right-to-left,
   after ignoring the rightmost 2 digits and generating always
   up to len elements t/nil."
  (loop
     for i = len then (1- i)
     for mode-number = (floor (/ instruction 100)) then (floor
                                                         (/ mode-number 10))
     while (plusp i)
     collect (= 1 (mod mode-number 10))))

(defun fetch-value (mem pc)
  (mem/r mem pc))

(defun fetch-addr (mem pc)
  (fetch-value mem (fetch-value mem pc)))

(defparameter *debug-stream* t)

(defun parse-op-params (mem pc instruction op)
  "Given that write operations are effectively performed as if in
  immediate mode, even if in the instructions it says it'll never
  be specified like that, we 'override' that choice in case
  it is an output arg."
  (let* ((n-args (op-n-args op))
         (modes  (instruction-modes instruction n-args))
         (output-arg (op-output-arg op))
         (param-values (loop
                          for immediate-mode-p in modes
                          for arg-idx below n-args
                          collect (if (or immediate-mode-p (= arg-idx output-arg))
                                      (fetch-value mem (+ arg-idx pc 1))
                                      (fetch-addr  mem (+ arg-idx pc 1))))))
    (format *debug-stream* "~%parse-op-params: ~A -> op(~A) mode: ~A -> ~A"
            (subseq mem pc (+ pc 1 n-args))
            (op-op-name op)
            modes
            param-values)
    param-values))
  



(defclass program-state ()
  ((program-memory
    :initarg :program-memory
    :accessor program-memory)
   (inputs
    :initarg :inputs
    :accessor inputs
    :initform nil)
   (outputs
    :initarg :outputs
    :accessor outputs
    :initform nil)
   (pc
    :documentation "Program counter. nil when execution finishes."
    :initarg :pc
    :accessor pc
    :initform 0))
  (:documentation "Holds an intcode program state."))

(defun compile-program (program-memory &key inputs)
  "Compile a program-state object"
  (make-instance 'program-state
                 :program-memory program-memory
                 :inputs inputs))

(defmethod is-done ((program-state program-state))
  (null (pc program-state)))


(defmethod compute ((program-state program-state) &key debug-stream)
  "If debug-stream is nil, no prints will be performed
   Special keywords are:
    :REWIND-OP-AND-PAUSE - jump move to the previous pc and :PAUSE
    :PAUSE - halt execution
    :EXIT - finish program (sets pc to nil)
    :JUMP - manipulate pc"
  (with-slots (program-memory inputs outputs pc) program-state
    (format debug-stream "~%~%Executing program ~A with size ~A (pc=~s).~%~%"
            program-memory (length program-memory) pc)
    (loop
       unless (is-done program-state)
       do
         (format debug-stream "Program Memory: ~a~%" program-memory)
         
       ;; exec op
         (let* ((instruction (mem/r program-memory pc))
                (opcode (instruction-opcode instruction))
                (op     (fetch-op opcode))
                (args   (parse-op-params program-memory pc instruction op))
                (op-output (apply (op-fn op) (concatenate 'list args (list :mem program-memory))))
                (num-parsed-addresses (1+ (op-n-args op)))) ; take the opcode itself into account.
           (format debug-stream " ->> ~S >>> ~A~%" (cons opcode args) op-output)
           
           ;; update pc & outputs on op-output
           (incf pc num-parsed-addresses)
           (typecase op-output
             (NULL)
             (SYMBOL (ecase op-output
                       (:REWIND-OP-AND-PAUSE (decf pc num-parsed-addresses))
                       (:PAUSE)
                       (:EXIT (setf pc nil)))
                     (loop-finish))
             (CONS (when (eql (first op-output) :JUMP)
                     (setf pc (second op-output))))
             (INTEGER (push op-output outputs))))
       finally
         (format debug-stream "Computed: ~A~%" outputs)
         (return (reverse outputs)))))
