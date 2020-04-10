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

(deftest input-comparison-programs
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
