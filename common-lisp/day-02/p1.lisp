;; helpers

(defun split-string (string &optional (delimiter #\Space))
  "Generic function to split a string by the delimiter."
  (do ((i-start 0)
       (i 0 (1+ i))
       (len (length string))
       (atoms nil))
      ((> i len) (reverse atoms))
    (when (or (= i len) (char= delimiter (aref string i)))
      (push (subseq string i-start i) atoms)
      (setf i-start (1+ i)))))

(defun read-program ()
  "Reads the program from disk as a list of integers"
  (flet ((split-to-integers (s)
           (mapcar #'parse-integer (split-string s #\,))))
     (split-to-integers (with-open-file (stream "input.txt")
                          (read-line stream)))))


(defparameter *program* (read-program))

(defun run (program)
  (do ((i 0 (+ i 4))
       (len (length program)))
      ((>= i len) program)
    (princ i)
    (let* ((opcode (nth i program))
           (op (cond ((= opcode 1) #'+)
                     ((= opcode 2) #'*)
                     ((= opcode 99) (return program))
                     (t (error (format t "Bad ASM: Received invalid op-code (~S)~%" opcode)))))
           (arg1 (nth (+ i 1) program))
           (arg2 (nth (+ i 2) program))
           (store-idx (nth (+ i 3) program))
           (val1 (nth arg1 program))
           (val2 (nth arg2 program)))
      (format t "> (~S ~S ~S ~S)     (~S -> [~S])~%"
              opcode arg1 arg2 store-idx
              (funcall op val1 val2) store-idx)
      (setf (nth store-idx program) (funcall op val1 val2)))))

(defun test-run (input expected-answer)
  (assert (equal (run input) expected-answer)))

(test-run '(1 0 0 0 99) '(2 0 0 0 99))
(test-run '(2 3 0 3 99) '(2 3 0 6 99))
(test-run '(2 4 4 5 99 0) '(2 4 4 5 99 9801))
(test-run '(1 1 1 4 99 5 6 0 99) '(30 1 1 4 2 5 6 0 99))

(let ((program (read-program)))
  ;; Change program state to before the accident:
  (setf (nth 1 program) 12
        (nth 2 program) 2)
  (run program))

(run (read-program))
