(in-package :cl-user)
(defpackage :aoc19-d7
  (:use :cl
        :aoc19-utils
        :cl-graph
        :aoc19-intcode
        :queues)) ; d5 provides the intcode computer code
(in-package :aoc19-d7)

(defparameter *debug-stream* nil)

(require :queues.simple-queue)

(register-op
 3 "op-3-input-op-phase-or-input"
 (lambda ($out &key $inputs)
   (let ((value (qpop $inputs)))
     (format *debug-stream* "~%~% [ Op3 fetching input value ~s ]~%" value)
     (if value (list :WRITE $out value)
         (list :REWIND-OP-AND-PAUSE)))))

                                        ; Program start

(defun inc-vector-array (array base)
  "Used to increment an array whose items act like the digits of base."
  (loop for idx downfrom (1- (length array)) to 0
     while (zerop
            (setf (elt array idx)
                  (mod (1+ (elt array idx)) base)))
     finally
       (return array)))

(defun unique-values-p (input-vector)
 "If there isn't any repeated element."
 (= (length input-vector)
    (length (remove-duplicates input-vector))))

(defun inc-phase (phase)
  "Increment phase vector with elements in modulo 5."
  (inc-vector-array phase 5))

(defparameter *program* '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))

(defparameter *program* '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0))

(defparameter *program* '(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0))

(defun setup-amplifier-chain (program-memory phase-values)
  (let ((amps (loop for i below 5 collecting
                   (compile-program program-memory))))
    (loop for i below 5
       do
         ;; set phase
         (qpush (inputs (elt amps i)) (elt phase-values i))
         ;; assign outputs of prev to next program's inputs
         (when (plusp i) (setf (outputs (elt amps (1- i)))
                               (inputs  (elt amps i))))
       finally
         (qpush (inputs (first amps)) 0)
         (return amps))))

(defparameter *outputs* nil)

(defparameter *program* (read-intcode-program "day7.txt"))

(defun amps-output (amps)
  (qpop (outputs (elt amps 4))))

(defun compute-best-amplifier-configuration (program)
  (let ((outputs nil) (phase (vector 0 0 0 0 0)))
    (loop for i below (expt 5 5)
       do
         (when (unique-values-p phase)
           (let ((amps (setup-amplifier-chain program phase)))
             (mapcar #'aoc-intcode:compute amps)
             (push (cons (copy-seq phase) (amps-output amps))
                   outputs)))
         (inc-phase phase))
    (sort outputs #'< :key #'cdr)))
