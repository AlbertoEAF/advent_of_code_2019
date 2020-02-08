(in-package :cl-user)
(defpackage :aoc19-utils
  (:use :cl)
  (:nicknames aoc-utils u)
  (:export #:let1
           #:alias
           #:hash-keys
           #:has-key))
(in-package :aoc19-utils)


(defmacro let1 (var value &body body)
  `(let ((,var ,value))
    ,@body))

(defun alias (new-name prev-name)
  (setf (symbol-function new-name) (symbol-function prev-name)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun has-key (hash-table key)
  (multiple-value-bind (_ found-p)
      (gethash key hash-table)
    (return-from has-key found-p)))
