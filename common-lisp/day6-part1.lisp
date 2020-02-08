;;(setf *default-pathname-defaults* #p"d:/web_programming/advent_of_code_2019/common-lisp/")
(in-package :cl-user)
(defpackage :aoc19-d6
  (:use :cl
        :aoc19-utils
        :cl-graph))
(in-package :aoc19-d6)

(ql:quickload "readable")
(readable:enable-sweet)

(defun readable:$bracket-apply$ (X at)
  (elt X at))

(defparameter *orbits*
  (mapcar (lambda (line)
            (uiop:split-string line :separator ")"))
          (uiop:read-file-lines "day6.txt")))

(defparameter *g* (init-graph *orbits*))

(defun init-graph (orbits)
  (let1 g (make-graph 'graph-container :default-edge-type :directed :test #'equal)
    (dolist (orbit orbits)
      (add-edge-between-vertexes g
                                 (intern (second orbit))
                                 (intern (first orbit))))
    g))



(defun collect-children (node)
  "Only implemented for single-children!"
  (if (has-children-p node)
      (let* ((direct-children (child-vertexes node))
             (direct-child (car direct-children)))
        (assert (< (length direct-children) 2))
        (cons direct-child (collect-children direct-child)))))

(defun graph-sources (graph)
  (find-vertexes-if graph (lambda (v)
                            (not (has-children-p v)))))


(defun sum-factorial (n)
  (loop for i to n summing i))

(defparameter *j* (init-graph '(
                                (C B)
                                (B A)
                                (A 0)
                                (G F)
                                (F E)
                                (E D))))
                               

(defparameter *root-path-lengths*
  (loop for v in (graph-roots *g*) collecting (length (collect-children v))))

(apply #'+ (mapcar #'sum-factorial *root-path-lengths*))

