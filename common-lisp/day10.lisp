(in-package :cl-user)
(defpackage :aoc19-d10
  (:use :cl
   :aoc19-utils
        :defclass-std
   :log4cl
   :iterate))
   
(in-package :aoc19-d10)
(load "lazy.lisp")

(defun read-map-lines (filepath)
  (uiop:read-file-lines filepath))

(defparameter *map-lines*
  (read-map-lines "day10.txt")
  "Raw map definition.")

(defun parse-lines (lines)
  (do* ((h (length lines))
        (w (length (first lines)))
        (table (make-array (list h w)))
        (row 0 (incf row))
        (line (first lines) (first rest))
        (rest (rest lines) (rest rest)))
       ((null line) table)
    (loop for col below w for char across line
       do (setf (aref table row col) char))))

(defparameter *map* (parse-lines *map-lines*))

(defun width (map)
  (array-dimension map 1))
(defun height (map)
  (array-dimension map 0))
(defun @ (map x y)
  (aref map y x))


#| Let's start with a dumb algorithm

For every point, select every other point and move towards it. If you find a hit, register that one,
and it's direction (rational number dx/dy).

When searching a target point, first see if that direction already has a hit.
If it has then it's already accounted for and there's no need to look there again.

|#

                                        ; point

(defstruct point x y)

(defmethod x ((pt point)) (point-x pt))
(defmethod y ((pt point)) (point-y pt))

(defun make-point-xy (x y)
  (make-point :x x :y y))

                                        ; vector

(defclass/std vec () ((x y)))

(defun direction (from to)
  (declare (point from to))
  (let* ((dx (- (x to) (x from)))
         (dy (- (y to) (y from)))
         (gcd (gcd dx dy)))
    (complex (/ dx gcd) (/ dy gcd))))

                                        ; code for this day

(defun make-hits ()
  (make-hash-table))

(defun asteroid-p (pt map)
  (char= #\# (@ map (x pt) (y pt))))

(defun add-hit (hits direction pt)
  (setf (gethash direction hits) pt))

(defun hit-in-direction-p (hits direction)
  (gethash direction hits))

(defun count-hits (hits)
  (hash-table-count hits))

(defun find-hit-towards (from to map hits)
  (do* ((direction (direction from to))
        (dx (realpart direction))
        (dy (imagpart direction))
        (x (x from) (incf x dx))
        (y (y from) (incf y dy))
        (pt (make-point :x x :y y)
            (make-point :x x :y y)))
       ((equalp pt to) (if (asteroid-p to map) (add-hit hits direction pt)))
    (if (asteroid-p pt map) (return (add-hit hits direction pt)))))

(defun count-visible-asteroids-from (from map)
  (let ((hits (make-hits)))
    (dotimes (y (height map))
      (dotimes (x (width map))
        (let ((pt (make-point :x x :y y)))
          (if (and (not (equalp from pt))
                   (asteroid-p pt map)
                   (not (hit-in-direction-p hits (direction from pt))))
              (find-hit-towards from pt map hits)))))
    (count-hits hits)))

(defun find-best-position (map)
  (iter
    (for i below (array-total-size map))
    (for from = (destructuring-bind (y x) (array-idx map i) (make-point :x x :y y)))
    (when (asteroid-p from map)
      (let ((count (count-visible-asteroids-from from map)))
        (finding (cons from count) maximizing count)))))


;; part1 tests:
(FIND-BEST-POSITION (parse-lines (read-map-lines "day10-t1.txt"))) ; => (#S(POINT :X 3 :Y 4) . 8)

;; part1:
(FIND-BEST-POSITION *map*)


                                        ; part 2 - vaporizing asteroids
