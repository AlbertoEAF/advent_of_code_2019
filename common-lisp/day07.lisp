(in-package :cl-user)
(defpackage :aoc19-d7
  (:use :cl
        :aoc19-utils
        :cl-graph
        :aoc19-intcode
        :queues))
(in-package :aoc19-d7)

(require :queues.simple-queue)


                                        ; Program start

(defparameter *program* (read-intcode-program "day7.txt"))

(register-op 3 "op-3-input-op-phase-or-input"
             (lambda ($out &key $inputs)
               (let ((value (qpop $inputs)))
                 (if value (list :WRITE $out value)
                     (list :REWIND-OP-AND-PAUSE)))))


(defun inc-digits (array start end)
  "Expands logic of incrementing a binary number's digits to any integer base and offset."
  (loop ; From least significant.
     for i downfrom (1- (length array)) to 0
     do ; Increment within range / reset.
       (if (= (1- end) (elt array i))
           (setf (elt array i) start)
           (setf (elt array i) (1+ (elt array i))))
     while ; There was carry over.
       (= (elt array i) start)))

(defun unique-values-p (input-vector)
 "If there isn't any repeated element."
 (= (length input-vector)
    (length (remove-duplicates input-vector))))

(defun link-amps (amps from to)
  (setf (outputs (elt amps from))
        (inputs  (elt amps to))))

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

(defun amps-output (amps)
  (qpop (outputs (elt amps 4))))

(defun all-amps-done (amps)
  (loop for amp across amps always (aoc-intcode::is-done amp)))

(defun compute-best-amplifier-configuration (program phase-start phase-end &key feedback)
  (let ((outputs nil)
        (phase (make-array 5 :initial-element phase-start))
        (phase-range (- phase-end phase-start)))
    (dotimes (i (expt phase-range phase-range))
      (when (unique-values-p phase)
        (let ((amps (setup-amplifier-chain program phase :feedback feedback)))
          (loop until (all-amps-done amps) do
               (loop for amp across amps do (aoc-intcode:compute amp)))
          (push (cons (copy-seq phase) (amps-output amps))
                outputs)))
      (inc-digits phase phase-start phase-end))
    (sort outputs #'< :key #'cdr)))


(compute-best-amplifier-configuration *program* 0 5) ; part 1
(compute-best-amplifier-configuration *program* 5 10 :feedback T) ; part 2
