(defun compute-fuel (mass)
  (- (floor (/ (parse-integer mass) 3)) 2))

(defun read-file-lines (filepath)
  (with-open-file (stream filepath)
    (do ((lines nil)
         (line (read-line stream nil) (read-line stream nil)))
        ((null line) (reverse lines))
      (push line lines))))

;; Sum all module's fuels:
(reduce #'+ (mapcar #'compute-fuel (read-file-lines "input.txt")))
