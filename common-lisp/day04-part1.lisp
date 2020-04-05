(require "asdf")
(asdf:load-system :readable)
(readable:enable-sweet) ; or whichever notation you've chosen.

(defun non-decreasing-digits-p (number-str)
  (dotimes (i (1- (length number-str)))
    (when (char> (elt number-str i)
                 (elt number-str (1+ i)))
      (return-from non-decreasing-digits-p nil)))
  T)

(defun has-digit-pair-p (number-str)
  (loop
     for i from 1 below (length number-str)
     when (char= (elt number-str i)
                 (elt number-str (1- i)))
     return T))

(defun valid-pass-p (number)
  (let ((number-str (write-to-string number)))
    (and (non-decreasing-digits-p number-str)
         (has-digit-pair-p number-str))))

(loop
   for i from 145852 to 616942
   summing (if (valid-pass-p i) 1 0))
