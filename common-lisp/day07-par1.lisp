(in-package :cl-user)
(defpackage :aoc19-d7
  (:use :cl
        :aoc19-utils
        :cl-graph
        :aoc19-intcode)) ; d5 provides the intcode computer code
(in-package :aoc19-d7)


(defvar *op3-calls* 0 "Track number of op3-calls per program")

(defun reset-op3-calls()
  (setf *op3-calls* 0))

(defun register-op3-call()
  (incf *op3-calls*))

(defparameter *op3-phase-params* (vector 0 0 0 0 0))

(defparameter *amplifier-input* 0)

(defparameter *debug-stream* nil)

(defun op3-input-op-phase-or-input ($out)
  (let ((value
         (if (evenp *op3-calls*) (elt *op3-phase-params* (/ *op3-calls* 2))
            *amplifier-input*)))
    (format *debug-stream* "~%~% [ Op3 fetching input value ~s (phase-input=~s) ]~%" value (evenp *op3-calls*))
    (register-op3-call)
    (list :WRITE $out value)))
    



(register-op 3 "op-3-input-op-phase-or-input"
             #'op3-input-op-phase-or-input)


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
         

(defparameter *outputs* nil)


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

(defun uses-only-unique-numbers (input-vector)
 "If there is any repeated digit then it is not valid"
 (= (length input-vector)
    (length (remove-duplicates input-vector))))


(progn
  (setf *outputs* nil)
  (reset-phase-params)
  
  (loop for i below (expt 5 2)
     do
       (loop while (not (uses-only-unique-numbers *op3-phase-params*))
          do (inc-vector-array *op3-phase-params* 5))
       (call-amplifiers-run-with-phase)
       (inc-vector-array *op3-phase-params* 5)))
  ;(print *outputs*))

(print *outputs*)

(setf *sorted* nil)
(defparameter *sorted* (sort *outputs* #'< :key (lambda (pair) (cdr pair))))
(print *sorted*)
(length *outputs*)
(length *sorted*)
