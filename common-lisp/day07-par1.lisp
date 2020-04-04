(in-package :cl-user)
(defpackage :aoc19-d7
  (:use :cl
        :aoc19-utils
        :cl-graph
        :aoc19-intcode
        :queues)) ; d5 provides the intcode computer code
(in-package :aoc19-d7)


(defparameter *op3-phase-params* (vector 0 0 0 0 0))

(defparameter *amplifier-input* 0)

(defparameter *debug-stream* nil)

(register-op 3 "op-3-input-op-phase-or-input"
             (lambda ($out &key $inputs)
               (let ((value (queues:qpop $inputs)))
                 (format *debug-stream* "~%~% [ Op3 fetching input value ~s ]~%" value)
                 (if value (list :WRITE $out value)
                     (list :REWIND-OP-AND-PAUSE)))))

                                        ; Program start




(defun reset-phase-params ()
  (setf *op3-phase-params* (vector 0 0 0 0 0)))

(defun set-phase-params (a b c d e)
  (setf *op3-phase-params* (vector a b c d e)))

(defun inc-vector-array (array base)
  "Used to increment an array whose items act like the digits of base."
  (loop for idx downfrom (1- (length array)) to 0
     while (zerop
            (setf (elt array idx)
                  (mod (1+ (elt array idx)) base)))
     finally
       (return array)))
         

(defun all-unique (input-vector)
 "If there is any repeated digit then it is not valid"
 (= (length input-vector)
    (length (remove-duplicates input-vector))))
                
             
(defun call-amplifiers-run-with-phase ()
  "Runs the computation for the 5 amplifiers"
  (setf *amplifier-input* 0)
  (reset-op3-calls)
  (loop
     for amplifier-index below 5
     do
       (setf *amplifier-input*
             (elt (compute (compile-program (copy-list *program*)) :debug-stream t) 0)))

  (push (cons (copy-seq *op3-phase-params*) *amplifier-input*)
        *outputs*))

(defparameter *program* '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))

(defparameter *program* '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0))

(defparameter *program* '(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0))

(defun setup-amplifier-chain (program-memory phase-values)
  (let ((amps (loop for i below 5 collecting
                   (compile-program program-memory))))
    (loop for i below 5
       do
         ;; set phase
         (queues:qpush (aoc-intcode::inputs (elt amps i)) (elt phase-values i))
         ;; assign outputs of prev to next program's inputs
         (when (plusp i) (setf (aoc-intcode::outputs (elt amps (1- i)))
                               (aoc-intcode::inputs  (elt amps i))))
       finally
         (queues:qpush (aoc-intcode::inputs (first amps)) 0)
         (return amps))))

(setf *amps*
  (make-amplifier-chain *program* *op3-phase-params*))

(defparameter *outputs* nil)

(progn
  (setf *outputs* nil)
  (reset-phase-params)
  
  (loop for i below (expt 5 3)
     do
       (loop while (not (all-unique *op3-phase-params*))
          do (inc-vector-array *op3-phase-params* 5))
       (let ((amps (setup-amplifier-chain *program* *op3-phase-params*)))
         (mapcar #'aoc-intcode:compute amps)
         (push (cons (copy-seq *op3-phase-params*)
                     (queues:qpop (aoc-intcode::outputs (elt amps 4))))
               *outputs*)
         (inc-vector-array *op3-phase-params* 5)))
  (print *outputs*))

(print *outputs*)

;; (setf *sorted* nil)
(defparameter *sorted* (sort *outputs* #'< :key (lambda (pair) (cdr pair))))
(print *sorted*)
;; (length *outputs*)
;; (length *sorted*)
