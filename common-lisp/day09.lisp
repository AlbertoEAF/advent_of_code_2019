(in-package :cl-user)
(defpackage :aoc19-d9
  (:use :cl
        :aoc19-utils
        :aoc19-intcode
        :queues))
(in-package :aoc19-d9)

(defparameter *program-mem* (read-intcode-program "day09.txt"))

(compute (compile-program *program-mem*))





