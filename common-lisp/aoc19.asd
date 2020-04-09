(defsystem "aoc19"
  :depends-on ("cl-interpol" "cl-graph" "readable" "trivial-arguments"
                             "queues" "log4cl" "array-operations"
                             "alexandria")
  :components ((:file "aoc19-utils")
               (:file "intcode")
               (:file "day05")))

(defsystem "aoc19/tests"
  :depends-on ("aoc19" "rove")
  :components ((:file "day05-tests")))
