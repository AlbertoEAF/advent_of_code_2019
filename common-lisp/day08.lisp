(in-package :cl-user)
(defpackage :aoc19-d8
  (:use :cl
        :aoc19-utils
        :cl-graph
        :queues
        :log4cl))
(in-package :aoc19-d8)

(ql:quickload :array-operations)

(defparameter *digits-string* (uiop:read-file-string "day08.txt"))

(defparameter shape '(25 6))

(log:config :debug)

(defun read-file-into-layers (filepath layer-w layer-h)
  (with-open-file (stream filepath)
    (do ((char (read-char stream) (read-char stream))
         (layer-size (* layer-w layer-h))
         (layers nil)
         (layer nil)
         (i nil))
        ((not (digit-char-p char)) layers)
      (when (null layer) (setf layer (make-array (list layer-w layer-h))
                               i 0))
      (setf (aref (aops:flatten layer) i) (digit-char-p char))
      (incf i)
      (when (= i layer-size) (progn (push layer layers)
                                    (setf layer nil
                                          i 0))))))

(defparameter *layers* (aoc19-d8::READ-file-into-layers "day08.txt" 25 6))

((lambda (layer) (let ((l (aops:flatten layer)))
                   (* (count 1 l) (count 2 l))))
 (find-best *layers* :key (lambda (layer) (count 0 (aops:flatten layer))))) ; part1
