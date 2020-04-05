(in-package :cl-user)
(defpackage :aoc19-d7
  (:use :cl
        :aoc19-utils
        :cl-graph
        :aoc19-intcode
        :queues)) ; d5 provides the intcode computer code
(in-package :aoc19-d7)

(require :queues.simple-queue)

(ql:quickload "readable")
(readable:enable-sweet)
(defun readable:$bracket-apply$ (X at)
  (elt X at))

(defparameter *debug-stream* nil)

(register-op
 3 "op-3-input-op-phase-or-input"
 (lambda ($out &key $inputs)
   (let ((value (qpop $inputs)))
     (format *debug-stream* "~%~% [ Op3 fetching input value ~s ]~%" value)
     (if value (list :WRITE $out value)
         (list :REWIND-OP-AND-PAUSE)))))

                                        ; Program start

(defun inc-digits (array start end)
  "Used to increment an array whose items act like the digits of base (end-start) with offset +start."
  (loop
     for idx downfrom (1- (length array)) to 0 ; From least significative.
     do ; increment
       (if (= (1- end) (elt array idx))
           (setf (elt array idx) start)
           (setf (elt array idx) (1+ (elt array idx))))
     until (/= start (elt array idx))) ; until we reach a digit that didn't need carry over.
  array)

(defun unique-values-p (input-vector)
 "If there isn't any repeated element."
 (= (length input-vector)
    (length (remove-duplicates input-vector))))

(defparameter *program* '(3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))

(defparameter *program* '(3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0))

(defparameter *program* '(3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0))

(defun link-amps (amps from to)
  (setf (outputs (elt amps from))
        (inputs (elt amps to))))

(defun set-amp-phase (amp phase-param)
  (qpush (inputs amp) phase-param))

(defun setup-amplifier-chain (program-memory phase &key feedback)
  (let ((amps (make-array 5)))
    (dotimes (i 5)
      ;; Initialize single amplifier with phase
      (setf (elt amps i) (compile-program program-memory))
      (set-amp-phase (elt amps i) (elt phase i))
      ;; assign outputs of prev to next program's inputs
      (when (plusp i)
        (link-amps amps (1- i) i)))
    (when feedback
      (link-amps amps 4 0))
    ;; Set phase 0 for the first amp
    (qpush (inputs (elt amps 0)) 0)
    amps))

(defparameter *outputs* nil)

(defparameter *program* (read-intcode-program "day7.txt"))

(defun amps-output (amps)
  (qpop (outputs (elt amps 4))))

(defun all-amps-done (amps)
  (loop for amp across amps always (aoc-intcode::is-done amp)))

(defun compute-best-amplifier-configuration (program phase-start phase-end &key feedback)
  (let ((outputs nil)
        (phase (make-array 5 :initial-element phase-start))
        (phase-range (- phase-end phase-start)))
    (loop for i below (expt phase-range phase-range)
       do
         (when (unique-values-p phase)
           (let ((amps (setup-amplifier-chain program phase :feedback feedback)))
             (loop until (all-amps-done amps) do
                  (loop for amp across amps do
                       (aoc-intcode:compute amp)))
             (push (cons (copy-seq phase) (amps-output amps))
                   outputs)))
         (inc-digits phase phase-start phase-end))
    (sort outputs #'< :key #'cdr)))


(compute-best-amplifier-configuration *program* 0 5) ; part 1
(compute-best-amplifier-configuration *program* 5 9 :feedback T) ; part 2
