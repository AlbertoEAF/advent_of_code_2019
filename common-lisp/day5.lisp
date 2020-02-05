(in-package :cl-user)
(defpackage :aoc19-d5
  (:use :cl)
  (:nicknames aoc-5)
  (:export
   #:compute
   #:register-op
   #:read-program))
(in-package :aoc19-d5)

;;;; helpers
;;(ql:quickload "cl-interpol")
;;(cl-interpol:enable-interpol-syntax)

(defun read-program ()
  "Reads the program from disk as a list of integers"
  (flet ((split-to-integers (s)
           (mapcar #'parse-integer (uiop:split-string s :separator ","))))
    (split-to-integers (uiop:read-file-string "day5.txt"))))


(defparameter *program* (read-program))

(defparameter *ops* (make-hash-table)
  "Stores the possible ops")

(defstruct op
  "output-arg will serve to override program-mode -> immediate mode for any   write argument."
  opcode
  n-args
  fn
  (output-arg -1))



(defun register-op (opcode n-args fn &key (output-arg -1))
  (setf (gethash opcode *ops*)
        (make-op :opcode opcode
                 :n-args n-args
                 :fn fn
                 :output-arg output-arg)))

(defun mem/r (memory address)
  (elt memory address))

(defun mem/w (memory address value)
  "Writes the value to memory at address. Returns nil."
  (setf (elt memory address) value)
  nil)

;; sum
(register-op 1 3
             (lambda (mem a b o)
               (mem/w mem o (+ a b)))
             :output-arg 3)

;; multiply
(register-op 2 3
             (lambda (mem a b o)
               (mem/w mem o (* a b)))
             :output-arg 3)

;; input
(register-op 3 1
             (lambda (mem o)
               (print mem)
               (format *query-io* "Program input:")
               (finish-output *query-io*)
               (mem/w mem o (parse-integer (read-line))))
             :output-arg 1)

;; output
(register-op 4 1
             (lambda (mem o)
               (declare (ignore mem))
               o))

;; jump-if-true
(register-op 5 2
             (lambda (mem a b)
               (declare (ignore mem))
               (when (/= 0 a)
                 (list :JUMP b))))

;; jump-if-false
(register-op 6 2
             (lambda (mem a b)
               (declare (ignore mem))
               (when (= 0 a)
                 (list :JUMP b))))

;; less-than
(register-op 7 3
             (lambda (mem a b o)
               (mem/w mem o
                      (if (< a b) 1 0)))
             :output-arg 3)

;; equals
(register-op 8 3
             (lambda (mem a b o)
               (mem/w mem o
                      (if (= a b) 1 0)))
             :output-arg 3)

(register-op 99 0
             (lambda (mem)
               (declare (ignore mem))
               :EXIT)) ; special SYMBOL to leave the program

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

(defun fetch-value (mem idx)
  (mem/r mem idx))

(defun fetch-addr (mem idx)
  (fetch-value mem (fetch-value mem idx)))

(defun parse-op-params (mem idx instruction op)
  "Given that write operations are effectively performed as if in
  immediate mode, even if in the instructions it says it'll never
  be specified like that, we 'override' that choice in case
  it is an output arg."
  (let* ((n-args (op-n-args op))
         (modes  (instruction-modes instruction n-args))
         (output-arg (op-output-arg op)))
    (format t "~%parse: ~A -> mode: ~A"
            (subseq mem idx (+ idx 1 n-args))
            modes)
    (loop
       for mode in modes
       for i from 1 to n-args
       collect (if (or mode (= i output-arg))
                   (fetch-value mem (+ i idx))
                   (fetch-addr  mem (+ i idx))))))


(defun compute (program)
  (format t "Executing program ~A with size ~A." program (length program))
  (loop
     with outputs
     for i below (length program)
     do
       (let* ((instruction (mem/r program i))
              (opcode (instruction-opcode instruction))
              (op     (fetch-op opcode))
              (args   (parse-op-params program i instruction op))
              (output (apply (op-fn op) (cons program args))))
         (format t " ->> ~S >>> ~A~%" (cons opcode args) output)
         (typecase output
           (SYMBOL (when (eql output :EXIT)
                     (loop-finish)))
           (CONS (when (eql (first output) :JUMP)
                   (setf i (second output))))
           (INTEGER (push output outputs)))
         (incf i (op-n-args op)))
     finally
       (format t "Computed: ~A~%" outputs)
       (finish-output *query-io*)
       (finish-output)
       (return (reverse outputs))))

(defparameter *test-out*
  (compute '(
             4 0 ;;1 2 5 4
             99)))

;(run (read-program))
