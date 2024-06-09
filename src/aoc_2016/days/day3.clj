(ns aoc-2016.days.day3
  (:require [clojure.string :as str]))

(declare is-valid-triangle to-int split-at-whitespace)

(defn solve []
  (let [lines (-> (slurp "./src/aoc_2016/days/day3_input.txt")
                  (str/trim)
                  (str/split-lines))
        triangles (map #(map to-int %) (map split-at-whitespace lines))
        p1 (count (filter is-valid-triangle triangles))]
    {:part1 p1}))

(defn is-valid-triangle [triangle]
  (let [[a b c] (sort triangle)]
    (> (+ a b) c)))

(defn split-at-whitespace [line]
  (-> line
      (str/trim)
      (str/split #"\s+")))

(defn to-int [n]
  (Integer/parseInt n))

(solve)
