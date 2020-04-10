;; (asdf:load-system "aoc19")

(in-package :cl-user)
(defpackage :aoc19-intcode
  (:use :cl
        :aoc19-utils
        :cl-interpol
        :trivial-arguments
        :queues)
  (:nicknames aoc-intcode)
  (:export
   #:compile-program
   #:compute
   #:register-op
   #:read-intcode-program
   #:mem/r
   #:mem/w
   #:inputs
   #:outputs))
(in-package :aoc19-intcode)

(require :queues.simple-queue)


(cl-interpol:enable-interpol-syntax)

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
  op-name
  requires-inputs)

(defun get-output-arg (fn)
 "Retrieves the output argument position."
 (position '$OUT (arg:arglist fn) :test #'string=))

(defun fn-requires-inputs (fn)
  "If it has a key argument called inputs"
  (let* ((args (arg:arglist fn)))
    (position '$inputs args
              :test #'string=
              :start (or (position '&key args)
                         (length args)))))


(defun register-op (opcode op-name fn)
  "The function can take as many arguments as needed, but:
   - $out is handled specially (interpreted as output address argument)
   - &key $inputs (optionally, if defined, the program inputs will be passed in).
  "
  (setf (gethash opcode *ops*)
        (make-op :opcode opcode
                 :n-args (aoc19-utils:get-function-mandatory-arguments-count fn)
                 :fn fn
                 :output-arg (or (get-output-arg fn) -1) ;; needs to be numerical
                 :op-name op-name
                 :requires-inputs (fn-requires-inputs fn))))

(defun mem/r (memory address)
  (elt memory address))

(defun mem/w (memory address value)
  "Writes the value to memory at address. Returns nil."
  (setf (elt memory address) value)
  nil)

(register-op 1 "+"
             (lambda (a b $out)
               (list :WRITE $out (+ a b))))

(register-op 2 "*"
             (lambda (a b $out)
               (list :WRITE $out (* a b))))

(register-op 3 "input"
             (lambda ($out)
               (format *query-io* "Program input:")
               (let ((input-value (parse-integer (read-line))))
                 (finish-output *query-io*)
                 (list :WRITE $out input-value))))

(register-op 4 "output"
             (lambda (a)
               (list :OUTPUT a)))

(register-op 5 "jump-if-true"
             (lambda (a b)
               (when (/= 0 a)
                 (list :JUMP b))))

(register-op 6 "jump-if-false"
             (lambda (a b)
               (when (= 0 a)
                 (list :JUMP b))))

(register-op 7 "<"
             (lambda (a b $out)
               (list :WRITE $out (if (< a b) 1 0))))

(register-op 8 "="
             (lambda (a b $out)
               (list :WRITE $out (if (= a b) 1 0))))

(register-op 99 "exit"
             (lambda ()
               (list :EXIT)))

(defun fetch-op (opcode)
  (let ((op (gethash opcode *ops*)))
    (if (null op)
        (error (format nil "Invalid/Unknown Opcode (~A)!" opcode))
        op)))

(defun instruction-opcode (instruction)
  (mod instruction 100))

(defun instruction-modes (instruction len)
  "Parses the instruction modes generating flags right-to-left,
   after ignoring the rightmost 2 digits and generating always
   up to len elements t/nil."
  (loop for _n-steps_ below len
     for digits = (truncate instruction 100) then (truncate digits 10)
     collect (mod digits 10)))

(defun fetch-value (mem pc)
  (mem/r mem pc))

(defun fetch-addr (mem pc)
  (fetch-value mem (fetch-value mem pc)))

(defun parse-op-params (mem pc instruction op relative-base debug-stream)
  "Given that write operations are effectively performed as if in
  immediate mode, even if in the instructions it says it'll never
  be specified like that, we 'override' that choice in case
  it is an output arg.
  Parameter modes:
  0: position mode - position (in memory). 50 => value=(read @50)
  1: immediate mode - value. 50 => 50
  2: relative mode - same as position but with relative base offset (starting at 0)
  "
  (let* ((n-args (op-n-args op))
         (modes  (instruction-modes instruction n-args))
         (output-arg (op-output-arg op))
         (param-values (loop
                          for mode in modes
                          for arg-idx = 0 then (1+ arg-idx)
                          ;; big computational performance regression here?!!
                          collect (if (and (/= 1 mode) (/= arg-idx output-arg))
                                      (fetch-addr  mem (+ arg-idx pc 1))
                                      (+ (fetch-value mem (+ arg-idx pc 1))
                                         (if (= 2 mode) relative-base 0))))))

    (format debug-stream "~%parse-op-params: ~A -> op(~A) mode: ~A w/=~A -> ~A"
            (subseq mem pc (+ pc 1 n-args))
            (op-op-name op)
            modes
            output-arg
            param-values)
    param-values))




(defclass program-state ()
  ((ram
    :initarg :ram
    :accessor ram)
   (pc
    :documentation "Program counter. nil when execution finishes."
    :initarg :pc
    :accessor pc
    :initform 0)
   (relative-base
    :documentation "Store relative offset for relative parameter mode (2)."
    :initarg :relative-base
    :accessor relative-base
    :initform 0)
   (inputs
    :initarg :inputs
    :accessor inputs
    :initform (queues:make-queue :simple-queue))
   (outputs
    :initarg :outputs
    :accessor outputs
    :initform (queues:make-queue :simple-queue))
   (debug-stream
    :documentation "For debugging purposes. Set to T to print logs to *standard-output*."
    :initarg :debug-stream
    :accessor debug-stream
    :initform nil))
  (:documentation "Holds an intcode program state."))

(defun compile-program (ram &key (ram-size 10000) debug-stream)
  "Compile a program-state object."
  (make-instance 'program-state
                 :ram (adjust-array (make-array (length ram) :initial-contents ram)
                                    (max (length ram) (or ram-size 0)))
                 :debug-stream debug-stream))

(defmethod is-done ((program-state program-state))
  (with-slots (pc ram) program-state
    (or (null pc)
        (= pc (length ram)))))

(defun compute-op-output (program)
  "Computes the output of calling op."
  (with-slots (ram pc relative-base inputs debug-stream) program
      (format debug-stream "Program Memory: ~a~%" ram)
    (let* ((instruction (mem/r ram pc))
           (opcode (instruction-opcode instruction))
           (op     (fetch-op opcode))
           (args   (parse-op-params ram pc instruction op relative-base debug-stream))
           (pc-increment (1+ (op-n-args op)))      ; Adds 1 for the opcode.
           (op-output (apply (op-fn op) (append args ; If needed pass extra-args.
                                                (if (op-requires-inputs op)
                                                    (list :$inputs inputs))))))
      (format debug-stream " ->> ~S >>> ~A~%"
              (cons opcode args) op-output)
      (values op-output pc-increment args op))))

(defun exec-op-output (program-state op-output pc-increment)
  "Applies the output of the operation to move the program interpreter state."
  (with-slots (ram inputs outputs pc) program-state
    (incf pc pc-increment)
    (when op-output
      (destructuring-bind (cmd &rest args) op-output
        (case cmd
          ;; non-halting commands
          (:OUTPUT (qpush outputs (first args)))
          (:WRITE (destructuring-bind (write-address write-value) args
                    (mem/w ram write-address write-value)))
          (:JUMP (setf pc (first args)))

          ;; all halting commands.
          (otherwise (ecase cmd
                       (:REWIND-OP-AND-PAUSE (decf pc pc-increment))
                       (:PAUSE)
                       (:EXIT (setf pc nil)))
                     (return-from exec-op-output T)))))

    (return-from exec-op-output nil))) ; signal continuation

(defun exec-pc-and-halt (program-state)
  "Runs a single op and signals termination with T. Returns nil otherwise."
  (multiple-value-bind (op-output pc-increment) (compute-op-output program-state)
    (exec-op-output program-state op-output pc-increment)))


(defmethod compute ((program program-state))
  "If debug-stream is nil, no prints will be performed
   Special keywords are:
    :WRITE - write at memory address a particular value
    :OUTPUT - saves a particular output
    :REWIND-OP-AND-PAUSE - jump move to the previous pc and :PAUSE
    :PAUSE - halt execution
    :EXIT - finish program (sets pc to nil)
    :JUMP - manipulate pc"
  (with-slots (ram outputs pc debug-stream) program
    (format debug-stream "~%~%Executing program ~A with size ~A (pc=~s).~%~%"
            ram (length ram) pc)
    (loop until (or (is-done program)
                    (exec-pc-and-halt program)))
    outputs))
