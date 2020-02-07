(defsystem "aoc2019"
  :depends-on ("cl-interpol")
  :components ((:file "aoc19-utils")
               (:file "day5")))


(defsystem "aoc2019/tests"
  :depends-on ("aoc2019" "rove")
  :components ((:file "day5-tests")))
