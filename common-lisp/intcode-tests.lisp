(in-package :cl-user)
(defpackage :aoc19-intcode-tests
  (:use :cl
        :aoc19-utils
        :cl-interpol
        :trivial-arguments
        :queues
        :rove
        :aoc19-intcode)
  (:shadowing-import-from :aoc19-intcode :outputs))
(in-package :aoc19-intcode-tests)

(defparameter *mock-input* :undefined-mock-input "Override this value to provide a custom input.")
(defparameter *mock-ops* (list
                          (def-op 3 "mock-input"
                            (lambda ($out)
                              (list :WRITE $out *mock-input*)))))

(defparameter *mock-program* nil
  "Place to store the current test program - for easier debug.")
(defparameter *s* nil "Stores the program source")


(defun mock/compile-program (program-mem)
  (setf *mock-program* (compile-program program-mem :with-ops *mock-ops* :ram-size 0)))

(defun outputs-1 (value outputs)
  (ok (= 1 (qsize outputs)))
  (ok (= value (qtop outputs))))
 
(defun q= (ref-output input)
  "Single-output is ref-output after passing input"
  (let ((*mock-input* input))
    (outputs-1 ref-output (compute (mock/compile-program *s*)))))

(let ((*mock-input* 8))
  (outputs-1 1 (compute (mock/compile-program '(3 9 8 9 10 9 4 9 99 -1 8)))))


(let ((*mock-input* 1))
  (compute (compile-program '(3 9 8 9 10 9 4 9 99 -1 8) :with-ops *mock-ops*)))

(deftest day5/input-comparison-programs
  (testing "input = 8 (position mode)"
    (setf *s* '(3 9 8 9 10 9 4 9 99 -1 8))
    (q= 0 7)
    (q= 1 8)
    (q= 0 9))

  (testing "input < 8 (position mode)"
    (setf *s* '(3 9 7 9 10 9 4 9 99 -1 8))
    (q= 1 6)
    (q= 1 7)
    (q= 0 8)
    (q= 0 9))

  (testing "input = 8 (immediate mode)"
    (setf *s* '(3 3 1108 -1 8 3 4 3 99))
    (q= 0 7)
    (q= 1 8)
    (q= 0 9))

  (testing "input < 8 (immediate mode)"
    (setf *s* '(3 3 1107 -1 8 3 4 3 99))
    (q= 1 6)
    (q= 1 7)
    (q= 0 8)
    (q= 0 9)))


;; day 9 tests

(defun get-queue-values (q)
  (loop for x = (qpop q) while x collecting x))

(deftest day9
  (testing "test program 1"
    (let* ((d9-p1-source (list 109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99))
           (p1 (compile-program d9-p1-source :ram-size 200)))
      (ok (equal d9-p1-source (get-queue-values (compute p1))))))
               
  (testing "test program 2"
    (ok (= 16
           (length (digits (qtop (compute (compile-program '(1102 34915192 34915192 7 4 7 99 0)))))))))

  (testing "test program 3"
    (let ((*s* '(104 1125899906842624 99)))
      (q= (second *s*) :any-input))))

