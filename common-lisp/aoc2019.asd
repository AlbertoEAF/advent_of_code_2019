(defsystem "aoc2019"
  :depends-on ("cl-interpol" "cl-graph" "readable" "trivial-arguments")
  :components ((:file "aoc19-utils")
               (:file "day5")
               (:file "day6-part1")))


(defsystem "aoc2019/tests"
  :depends-on ("aoc2019" "rove")
  :components ((:file "day5-tests")))
