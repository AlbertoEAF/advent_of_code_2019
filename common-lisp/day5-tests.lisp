(in-package :cl-user)
(ql:quickload "rove")
(defpackage aoc-d5-test
  (:use :cl
        :aoc19-d5
        :rove))
(in-package :aoc-d5-test)


(deftest prove-test
  (testing "blah"
    (ok (not (find 4 '(1 2 3))))
    (ok (= 4 4))
    (ok (equal 1 #\1))))

#|
(run-suite *package*)

(defparameter *test-out*
  (aoc-5::compute '(
                    4 0 ;;1 2 5 4
                    99)))

;; input == 8
(aoc-5::compute '(3 9 8 9 10 9 4 9 99 -1 8))

;; input < 8
(compute '(3 9 7 9 10 9 4 9 99 -1 8))

;; input == 8 (immediate mode)
(compute '(3 3 1108 -1 8 3 4 3 99))

(compute (read-program))

;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;

(defparameter *test-input* nil)

(aoc-5::register-op 3 1
                    (lambda (mem o)
                      (setf (elt mem o) *test-input*))
                    :output-arg 1)

(defun with-input-compute (input intcode-program)
  (setf *test-input* input)
  (aoc-5::compute intcode-program))

(setf *test-input* 8)

(ng (= 1 1))


(let ((p '(3 9 8 9 10 9 4 9 99 -1 8)))
  (assert (= 1 (with-input-compute 8 p))))

|#
