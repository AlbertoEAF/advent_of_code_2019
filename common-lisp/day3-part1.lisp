(defun read-wires-cmds ()
  (mapcar (lambda (line)
            (uiop:split-string line :separator ","))
          (uiop:read-file-lines #p"day3.txt")))

(defstruct pt
  (x 0) (y 0))

(defstruct segment
  (p1 (make-pt))
  (p2 (make-pt)))

(defmethod add (p1 p2)
  (make-pt :x (+ (pt-x p1) (pt-x p2))
           :y (+ (pt-y p1) (pt-y p2))))

(defun make-move (move-instruction)
  (let ((first-char (elt move-instruction 0))
        (offset (parse-integer (subseq move-instruction 1))))
    (when (zerop offset)
      (error "Tring to build segment with no size!"))
    (ccase first-char
      (#\R (make-pt :x offset))
      (#\L (make-pt :x (- offset)))
      (#\U (make-pt :y offset))
      (#\D (make-pt :y (- offset))))))

(defun make-segment-with-move-cmd (origin move-cmd)
  (make-segment :p1 origin
                :p2 (add origin (make-move move-cmd))))

(defun build-wire-from-move-cmds (wire-cmds)
  (do* ((segments nil)
        (new-segment nil)
        (end-pt (make-pt))
        (todo-cmds wire-cmds (cdr todo-cmds))
        (cmd (car todo-cmds) (car todo-cmds)))
       ((null cmd) segments)
    (setf new-segment (make-segment-with-move-cmd end-pt cmd))
    (setf end-pt (segment-p2 new-segment))
    (push new-segment segments)))

(defun is-horizontal (segment)
  (= (pt-y (segment-p1 segment))
     (pt-y (segment-p2 segment))))

(defun is-vertical (segment)
 "As there are no 0-length segments, this is the logic complement of is-horizontal"
 (not (is-horizontal segment)))

(defun segment-pts (segment)
  (list (segment-p1 segment) (segment-p2 segment)))

(defun segment-range (segment pt-accessor-fn)
  "Extracts a sorted list of 2 numbers (both x's or y's from the segment)"
  (sort (mapcar pt-accessor-fn (segment-pts segment)) #'<))

(defun segment-x-range (segment)
  (segment-range segment #'pt-x))

(defun segment-y-range (segment)
  (segment-range segment #'pt-y))

(defun ranges-cmp (range1 range2)
  (<= (first range1) (first range2)))

(defun sort-ranges (&rest ranges)
  (sort ranges #'ranges-cmp))

(defun range-overlap (range1 range2) ; range is a sorted list (start end)
  (destructuring-bind ((a1 b1) (a2 b2)) (sort-ranges range1 range2)
    (when (>= b1 a2)
      (list (max a1 a2)
            (min b1 b2)))))

(defun intersects (segment1 segment2)
  (and (range-overlap (segment-x-range segment1)
                      (segment-x-range segment2))
       (range-overlap (segment-y-range segment1)
                      (segment-y-range segment2))))

(defun L1-cmp (a b)
  (<= (abs a) (abs b)))

(defun range-crosses-zero-p (range)
  (and (<= 0 (first range))
       (>= 0 (second range))))

(defun range-closest-point-to-0 (range)
  (if (range-crosses-zero-p range)
      ; crosses 0? use 0
      0
      ; doesn't cross 0? use the smallest L1 distance to 0
      (first (sort range #'L1-cmp))))



(defun closest-intersection-to-origin (segment1 segment2)
  "If there's an intersection point return the closest to the
   origin. Otherwise, return NIL."
  (let* ((x-overlap (range-overlap (segment-x-range segment1)
                                   (segment-x-range segment2)))
         (y-overlap (range-overlap (segment-y-range segment1)
                                   (segment-y-range segment2))))
    (when (and x-overlap y-overlap)
      (mapcar #'range-closest-point-to-0
              (list x-overlap y-overlap)))))


(defparameter *square* (BUILD-WIRE-FROM-MOVE-CMDS (list "R12" "U12" "L12" "D12")))

(closest-intersection-to-origin (elt *square* 0) (elt *square* 1))

(defparameter *s1* (make-segment :p1 (make-pt :x 0  :y 1)
                                 :p2 (make-pt :x 10 :y 1)))

(defparameter *s2* (make-segment :p1 (make-pt :x 12 :y 1)
                                 :p2 (make-pt :x 15 :y 1)))

(defparameter *s3* (make-segment :p1 (make-pt :x 10 :y -5)
                                 :p2 (make-pt :x 10 :y 15)))


(defparameter *wires*
  (mapcar #'BUILD-WIRE-FROM-MOVE-CMDS (read-wires-cmds)))

(defparameter *wire1* (first *wires*))
(defparameter *wire2* (second *wires*))

(defun get-intersections (wire1 wire2)
  (loop
     with intersection = nil
     for s1 in wire1 appending
       (loop for s2 in wire2
          do
            (setf intersection (closest-intersection-to-origin s1 s2))
          when intersection
          collect intersection)))

(defun get-intersections-distances (wire1 wire2)
 (sort (mapcar #'L1-distance
               (get-intersections wire1 wire2))
       #'<))

(defun L1-distance (pair)
  (+ (abs (first pair)) (abs (second pair))))


(get-intersections-distances *wire1* *wire2*)

;; ex 1

(defparameter *wire1* (BUILD-WIRE-FROM-MOVE-CMDS
                       (list "R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72")))

(defparameter *wire2* (BUILD-WIRE-FROM-MOVE-CMDS
                       (list "U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83")))

= distance 159
(get-intersections-distances *wire1* *wire2*)

;; ex 2

(get-intersections-distances (build-wire-from-move-cmds
                              (list "R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"))
                             (build-wire-from-move-cmds
                              (list "U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7")))
