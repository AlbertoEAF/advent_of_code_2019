(in-package :cl-user)
(defpackage :aoc19-utils
  (:use :cl)
  (:nicknames aoc-utils u)
  (:export
   #:let1))
(in-package :aoc19-utils)


(defmacro let1 (var value &body body)
  `(let ((,var ,value))
    ,@body))

(defun alias (new-name prev-name)
  (setf (symbol-function new-name) (symbol-function prev-name)))
