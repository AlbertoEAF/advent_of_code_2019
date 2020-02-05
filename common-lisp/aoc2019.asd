(defsystem "aoc2019"
    :components ((:file "day5")))


(defsystem "aoc2019/tests"
    :depends-on ("aoc2019" "rove")
    :components ((:file "day5-tests")))
