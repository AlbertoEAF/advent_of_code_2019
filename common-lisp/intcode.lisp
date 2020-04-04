                                        ; (asdf:load-system "aoc2019")

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
 (position '$OUT (arg:arglist fn)))

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
                 :program-memory (copy-list program-memory)
                 :inputs inputs))

(defmethod is-done ((program-state program-state))
  (with-slots (pc program-memory) program-state
    (or (null pc)
        (= pc (length program-memory)))))

(defun compute-op-output (program-memory pc inputs &key debug-stream)
  "Computes the output of calling op."
  (format debug-stream "Program Memory: ~a~%" program-memory)
  (let* ((instruction (mem/r program-memory pc))
         (opcode (instruction-opcode instruction))
         (op     (fetch-op opcode))
         (args   (parse-op-params program-memory pc instruction op))
         (pc-increment (1+ (op-n-args op))) ; Adds 1 for the opcode.
         (op-output (apply (op-fn op) (append args ; If needed pass extra-args.
                                              (if (op-requires-inputs op)
                                                  (list :$inputs inputs))))))
    ;; (concatenate 'list args (list :mem program-memory)))))
    (format debug-stream " ->> ~S >>> ~A~%"
            (cons opcode args) op-output)
    (values op-output pc-increment args op)))

(defun exec-op-output (program-state op-output pc-increment)
  "Applies the output of the operation to move the program interpreter state."
  (with-slots (program-memory inputs outputs pc) program-state
    (incf pc pc-increment)
    (when op-output
      (destructuring-bind (cmd &rest args) op-output
        (case cmd
          ;; non-halting commands
          (:OUTPUT (push (first args) outputs))
          (:WRITE (destructuring-bind (write-address write-value) args
                    (mem/w program-memory write-address write-value)))
          (:JUMP (setf pc (first args)))

          ;; all halting commands.
          (otherwise (ecase cmd
                       (:REWIND-OP-AND-PAUSE (decf pc pc-increment))
                       (:PAUSE)
                       (:EXIT (setf pc nil)))
                     (return-from exec-op-output T)))))

    (return-from exec-op-output nil))) ; signal continuation


(defmethod compute ((program-state program-state) &key debug-stream)
  "If debug-stream is nil, no prints will be performed
   Special keywords are:
    :WRITE - write at memory address a particular value
    :OUTPUT - saves a particular output
    :REWIND-OP-AND-PAUSE - jump move to the previous pc and :PAUSE
    :PAUSE - halt execution
    :EXIT - finish program (sets pc to nil)
    :JUMP - manipulate pc"
  (with-slots (program-memory inputs outputs pc) program-state
    (format debug-stream "~%~%Executing program ~A with size ~A (pc=~s).~%~%"
            program-memory (length program-memory) pc)
    (loop
       until (is-done program-state)
       do
         (multiple-value-bind (op-output pc-increment)
             (compute-op-output program-memory pc inputs :debug-stream debug-stream)
           (when (exec-op-output program-state op-output pc-increment)
             (loop-finish)))
       finally
         (format debug-stream "Computed: ~A~%" outputs)
         (return (reverse outputs)))))
