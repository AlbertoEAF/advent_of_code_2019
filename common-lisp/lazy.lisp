;; http://blog.thezerobit.com/2012/07/28/lazy-sequences-in-common-lisp.html SICP lazy seqs

(defstruct lseq value next)

(defun lgen (&optional value next-transform stop-test)
  (make-lseq :value value
             :next (if (and next-transform
                            (or (not stop-test) (not (funcall stop-test value))))
                       (lambda () (lgen (funcall next-transform value)
                                        next-transform
                                        stop-test)))))

(defmethod lget ((lseq lseq))
  (lseq-value lseq))

(defmethod lget ((lseq null))
  (error "End-of-iteration"))

(defmethod ldone-p ((lseq lseq))
  (null (lseq-next lseq)))

(defmethod lnext ((lseq lseq))
  (if (ldone-p lseq) (error "End-of-iteration")
      (funcall (lseq-next lseq))))

(defmethod tolist ((lseq lseq))
  (loop for seq = lseq then (lnext seq)
        collecting (lget seq)
        while (not (ldone-p seq))))

;; convenience generators

(defun lrange (&optional (start 0) (step 1) (end nil end-p))
  (lgen start
        (lambda (x) (+ x step))
        (if end-p (lambda (x) (= x (- end step))))))

(defun larray-major-idx (array)
  (lrange 0 1 (array-total-size array)))

(defun gen-row-major-array-idxs-sequence (array)
  (let ((dims (array-dimensions array))
        (rank (array-rank array)))))

(defun array-idx (array row-major-array-idx)
  "Turns a row-major index into the indexes of the array"
  (let ((out-idxs))
    (loop
      for row-major-rest-idx = row-major-array-idx then rest-idx
      for dim below (array-rank array)
      for current-array-dim = (car (array-dimensions array)) then (car rest-array-dims)
      for rest-array-dims = (cdr (array-dimensions array)) then (cdr rest-array-dims)

      for (idx rest-idx) = (multiple-value-list (truncate row-major-rest-idx
                                                          (apply #'* rest-array-dims)))
      do
         (push idx out-idxs))
    (nreverse out-idxs)))
