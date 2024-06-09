(ns aoc-2016.days.day3
  (:require [clojure.string :as str])
  (:require [aoc-2016.aoc-utils :as utils]))

(declare is-valid-triangle)

(defn solve []
  (let [triangles (->>
                   (utils/load-input "day3")
                   (map utils/split-at-whitespace)
                   (map (fn [line] (map #(Integer/parseInt %) line))))
        p1 (->>
            triangles
            (filter is-valid-triangle)
            count)
        p2 (->>
            triangles
            utils/transpose
            flatten
            (partition-all 3)
            (filter is-valid-triangle)
            count)]
    {:part1 p1 :part2 p2}))

(defn is-valid-triangle [triangle]
  (let [[a b c] (sort triangle)]
    (> (+ a b) c)))

(solve)
