(ql:quickload "cl-interpol")
;; helpers
(cl-interpol:enable-interpol-syntax)

(defun read-program ()
  "Reads the program from disk as a list of integers"
  (flet ((split-to-integers (s)
           (mapcar #'parse-integer (uiop:split-string s :separator ","))))
    (split-to-integers (uiop:read-file-string "day5.txt"))))


(defparameter *program* (read-program))

#|
This program supports the following opcodes:

01 a b c : {@a + @b} -> @c
02 a b c : {@a + @b} -> @c
03 a     : (input) -> @a
04 a     : (output @a)
99       : stop

And parameter modes (binary opcode prefix):

# Parameter modes read from right-to-left:

- Parameter Mode 0 : Position mode (00)
   Each parameter is an address

- Parameter Mode 1 : Immediate mode
   Each parameter is a value

|#

(defparameter *ops* (make-hash-table)
  "Stores the possible ops")

(defstruct op
  opcode
  n-args
  fn)



(defun register-op (opcode n-args fn)
  (setf (gethash opcode *ops*)
        (make-op :opcode opcode
                 :n-args n-args
                 :fn fn)))

(defun mem/r (memory address)
  (elt memory address))

(defun mem/w (memory address value)
  "Writes the value to memory at address. Returns nil."
  (setf (elt memory address) value)
  nil)

(register-op 1 3
             (lambda (mem a b o)
               (mem/w mem o (+ a b))))

(register-op 2 3
             (lambda (mem a b o)
               (mem/w mem o (* a b))))

(register-op 3 1
             (lambda (mem o)
               (format *query-io* "Program input:")
               (finish-output *query-io*)
               (mem/w mem o (parse-integer (read-line)))))

(register-op 4 1
             (lambda (mem o)
               (declare (ignore mem))
               o))

(register-op 99 0
             (lambda (mem)
               (declare (ignore mem))
               :EXIT)) ; special SYMBOL to leave the program

(defun fetch-op (opcode)
  (let ((op (gethash opcode *ops*)))
    (if (null op)
        (error (format nil "Invalid/Unknown Opcode (~A)!" opcode))
        op)))

(defun instruction-opcode (instruction)
  (mod instruction 100))

(defun length-integer (n)
  "Log-base-10 + ceiling of an integer + fractional part gives the #digits."
  (ceiling (log (+ n 0.1) 10)))

(defun instruction-modes (instruction len)
  "Parses the instruction modes generating flags right-to-left,
   after ignoring the rightmost 2 digits and generating always
   up to len elements t/nil."
  (loop
     for i = len then (1- i)
     for mode-number = (floor (/ instruction 100)) then (floor
                                                         (/ mode-number 10))
     while (plusp i)
     collect (= 1 (mod mode-number 10))))

(defun fetch-value (mem idx)
  (mem/r mem idx))

(defun fetch-addr (mem idx)
  (fetch-value mem (fetch-value mem idx)))

(defun parse-op-params (mem idx instruction op)
  (let* ((n-args (op-n-args op))
         (modes  (instruction-modes instruction n-args)))
    (print (subseq mem (1+ idx) (+ idx 1 n-args)))
    (print modes)
    (loop
       for mode in modes
       for i from 1 to n-args
       collect (if mode (fetch-value mem (+ i idx))
                        (fetch-addr  mem (+ i idx))))))
                               

(defun test (program)
  (loop
     for i = 0 then (1+ i)
     while (< i (length program))
     collecting
       (let* ((instruction (mem/r program i))
              (opcode (instruction-opcode instruction))
              (op     (fetch-op opcode))
              (args   (parse-op-params program i instruction op))
              (lol (format t "~% ~S~S ~%" opcode args))
              (n-args (op-n-args op))
              (output (apply (op-fn op) (cons program args))))
         (if (eql output :EXIT)
             (break))
         ;(print (list (subseq program i) output))
         (incf i n-args))))

(test '(3 1 1 2 5 4 99))

(test (read-program))

(test '(1101 100 -1 4 0))
