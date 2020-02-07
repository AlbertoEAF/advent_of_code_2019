(in-package :cl-user)
(ql:quickload "rove")
(defpackage aoc19-d5-tests
  (:use :cl
        :aoc19-utils
        :aoc19-d5
        :rove))
(in-package :aoc19-d5-tests)


;; we first mock the input op (3) to use as input a value in *test-input*

(defparameter *test-input* nil
  "This value is used as input value (mock)")

(defparameter *p* nil
  "Program global variable")


(register-op 3 1
             (lambda (mem o)
               (mem/w mem o *test-input*))
             :output-arg 1)

(defun compute-1-input (input)
  (setf *test-input* input)
  (compute (copy-seq *p*)))

(alias 'f 'compute-1-input)

(defun outputs-1 (input-value output-value)
  (ok (equal `(,output-value) (f input-value))))

(defun f=1 (input-value)
  (outputs-1 input-value 1))

(defun f=0 (input-value)
  (outputs-1 input-value 0))


(deftest input-comparison-programs
  (testing "input = 8 (position mode)"
    (setf *p* '(3 9 8 9 10 9 4 9 99 -1 8))
    (f=0 8)
    (f=0 7)
    (f=0 9))

  (testing "input < 8 (position mode)"
    (setf *p* '(3 9 7 9 10 9 4 9 99 -1 8))
    (f=1 7)
    (f=0 8)
    (f=0 9))

  (testing "input = 8 (immediate mode)"
    (setf *p* '(3 3 1108 -1 8 3 4 3 99))
    (f=1 8)
    (f=0 7)
    (f=0 9))

  (testing "input < 8 (immediate mode)"
    (setf *p* '(3 3 1107 -1 8 3 4 3 99))
    (f=1 7)
    (f=0 8)
    (f=0 9)))


(deftest jump-tests
  (testing "non-zero input? 1/0 (position mode)"
    (setf *p* '(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9))
    (testing ">0 cases"
      (f=1 1)
      (f=1 2)
      (f=1 5)
      (f=1 15))
    (testing "<0 cases"
      (f=1 -1)
      (f=1 -2)
      (f=1 -3)
      (f=1 -49))
    (testing "=0 case"
      (f=0 0)))

  (testing "non-zero input? 1/0 (immediate mode)"
    (setf *p* '(3 3 1105 -1 9 1101 0 0 12 4 12 99 1))
    (testing ">0 cases"
      (f=1 1)
      (f=1 2)
      (f=1 5)
      (f=1 15))
    (testing "<0 cases"
      (f=1 -1)
      (f=1 -2)
      (f=1 -3)
      (f=1 -49))
    (testing "=0 case"
      (f=0 0))))
