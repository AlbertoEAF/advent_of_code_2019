(setf *default-pathname-defaults* #p"d:/web_programming/advent_of_code_2019/common-lisp/")

(defparameter *orbits*
  (mapcar (lambda (line)
            (uiop:split-string line :separator ")"))
          (uiop:read-file-lines "day6.txt")))

(defstruct graph
  (nodes (make-hash-table :test #'equal))
  (edges nil))

(defparameter *g* (make-graph))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun has-key (hash-table key)
  (multiple-value-bind (_ found-p)
      (gethash key hash-table)
    (return-from has-key found-p)))

(defun find-roots (g)
  "Finds the root nodes in a directed graph"
  (loop
     with edges = (graph-edges g)
     with roots = nil
     with non-roots = (make-hash-table :size (hash-table-size (graph-nodes g)))
     for edge in edges
     do
       (setf (gethash (second edge) non-roots) t)
     finally
       (loop with nodes = (hash-keys (graph-nodes g))
          for node in nodes
          do
            (if (not (has-key non-roots node))
                (push node roots)))
       (return roots)))

(defun add-node (g key &optional value)
  (setf (gethash key (graph-nodes g)) value))

(defun add-edge (g a b)
  (push (list a b) (graph-edges g)))


(defun init-input-graph ()
  (loop
     with g = (make-graph)
     for line in *orbits*
     do
       (add-edge g (second line) (first line))
       (add-node g (first line))
       (add-node g (second line))
     finally
       (return g)))

(defun count-edges-to-end (g start-node)
  ())


(defparameter *g*
  (init-input-graph))
