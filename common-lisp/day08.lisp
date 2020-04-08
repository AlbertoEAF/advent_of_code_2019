(in-package :cl-user)
(defpackage :aoc19-d8
  (:use :cl
        :aoc19-utils
        :queues
        :log4cl
        :array-operations))
(in-package :aoc19-d8)
(require 'alexandria)
                                        ; Layers of size Width=25 x Height=6

(defun read-stream-layers (stream layer-h layer-w)
  (do ((char (read-char stream) (read-char stream))
       (layer-size (* layer-w layer-h))
       (layers nil)
       (layer nil)
       (i nil))
      ((not (digit-char-p char)) (nreverse layers))
    (when (null layer) (setf layer (make-array (list layer-h layer-w))
                             i 0))
    (setf (row-major-aref layer i) (digit-char-p char))
    (incf i)
    (when (= i layer-size) (progn (push layer layers)
                                  (setf layer nil
                                        i 0)))))

(defparameter *layers* (with-open-file (stream "day08.txt")
                         (read-stream-layers stream 6 25)))

                                        ; part 1

((lambda (layer) (let ((l (aops:flatten layer)))
                   (* (count 1 l) (count 2 l))))
 (find-best *layers* :key (lambda (layer) (count 0 (aops:flatten layer)))))


                                        ; part 2

(defun decode-layers (layers)
  (loop
     with image = (alexandria:copy-array (first layers))
     for i below (array-total-size image)
     do
       (setf (row-major-aref image i) (dolist (layer layers 2) ; return 2 if no non-transparent value arose
                                        (let ((value (row-major-aref layer i)))
                                          (when (< value 2) (return value)))))
     finally
       (return image)))
       

(defun print-image (img)
  (loop
     with layer-h = (elt (array-dimensions img) 0)
     with layer-w = (elt (array-dimensions img) 1)
     for y below layer-h
     do
       (loop for x below layer-w do
            (ecase (aref img y x)
              (1 (princ #\*))
              (0 (princ #\Space))))
       (princ #\Newline)))

(print-image
 (decode-layers *layers*))


