(defsystem "aoc19"
  :depends-on ("cl-interpol" "cl-graph" "readable" "trivial-arguments"
                             "queues")
  :components ((:file "aoc19-utils")
               (:file "day05")
               (:file "day06-part1")
               (:file "intcode")))


(defsystem "aoc19/tests"
  :depends-on ("aoc19" "rove")
  :components ((:file "day05-tests")))
