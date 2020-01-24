(defun compute-fuel (mass)
  "Fuel needed for a certain mass"
  (- (floor (/ mass 3)) 2))

(defun compute-fuel-needed (mass)
  "Fuel needed for a certain mass + the fuel needed to carry that fuel (and so on...)."
  (do ((total-fuel 0)
       (added-fuel (compute-fuel mass) (compute-fuel added-fuel)))
      ((<= added-fuel 0) total-fuel)
    (incf total-fuel added-fuel)))

(defun read-file-lines (filepath)
  (with-open-file (stream filepath)
    (do ((lines nil)
         (line (read-line stream nil) (read-line stream nil)))
        ((null line) (reverse lines))
      (push line lines))))

;; Sum all module's fuels:
(reduce #'+ (mapcar #'compute-fuel-needed
                    (mapcar #'parse-integer (read-file-lines "input.txt"))))
