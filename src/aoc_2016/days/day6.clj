(ns aoc-2016.days.day6
  (:require
   [aoc-2016.aoc-utils :as utils]))

(defn solve []
  (let [input (utils/load-input "day6")
        columns (utils/transpose input)
        p1 (->> columns
                (map utils/most-common)
                keys
                (apply str))
        p2 (->> columns
                (map utils/least-common)
                keys
                (apply str))]
    {:part1 p1 :part2  p2}))

(solve)

