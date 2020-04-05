(require "asdf")
(asdf:load-system :readable)
(readable:enable-sweet)

(defun READABLE:$BRACKET-APPLY$ (iter at)
  (elt iter at))

(defun non-decreasing-digits-p (number-str)
  (dotimes (i (1- (length number-str)))
    (when (char> (elt number-str i)
                 (elt number-str (1+ i)))
      (return-from non-decreasing-digits-p nil)))
  T)

(defun longest-chain-length-at (s start &optional (end (length s)))
  "Returns the longest chain length of the same character"
  (loop for i from (1+ start) below end
     while (char= s[start] s[i])
     finally (return (- i start))))

;; not needed for this problem:
(defun longest-chain-length (s)
  "Longest chain of the same character in the sequence."
  (loop with L = (length s) for i below L maximizing
       (longest-chain-length-at s i L)))

(defun has-chain-one-pair-p (s)
  (loop
     for chain-size = 0 then (longest-chain-length-at s i)
     for i          = 0 then (+ i chain-size)
     while (and (< i (length s))
                (/= 2 chain-size))
     finally (return (= 2 chain-size))))


(defun valid-pass-p-part2 (number)
  (let ((number-str (write-to-string number)))
    (and (non-decreasing-digits-p number-str)
         (has-chain-one-pair-p number-str))))

(loop
   for i from 145852 to 616942
   summing (if (valid-pass-p-part2 i) 1 0)) ; not 816
