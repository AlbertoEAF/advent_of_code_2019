(in-package :cl-user)
(defpackage :aoc19-d8
  (:use :cl
        :aoc19-utils
        :cl-graph
        :aoc19-intcode
        :queues))
(in-package :aoc19-d8)

(defparameter *digits-string* (uiop:read-file-string "day08.txt"))

(defparameter shape '(25 6))

(defun read-file-into-layers (filepath layer-w layer-h)
  (with-open-file (stream filepath)
    (do ((char (read-char stream) (read-char stream))
         (layer-size (* layer-w layer-h))
         (layers nil)
         (layer nil)
         (i nil))
        ((not (digit-char-p char)) layers)
      (when (null layer) (setf layer (make-array layer-size)
                               i 0))
      (setf (aref layer i) (digit-char-p char))
      (incf i)
      (when (= i layer-size) (progn (push layers layer)
                                    (setf layer nil
                                          i 0))))))
      
