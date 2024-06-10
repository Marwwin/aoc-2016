(ns aoc-2016.aoc-utils
  (:require [clojure.string :as str]))

(defn load-input [day]
  (-> (slurp (str "./src/aoc_2016/days/" day "_input.txt"))
      (str/trim)
      (str/split-lines)))

(defn most-common [coll]
  (apply max-key val (frequencies coll)))

(defn least-common [coll]
  (apply min-key val (frequencies coll)))

(defn manhattan-2d
  [vec1  vec2]
  (+
   (Math/abs (- (vec2 :x) (vec1 :x)))
   (Math/abs (- (vec2 :y) (vec1 :y)))))

(defn transpose [triangles]
  (apply mapv vector triangles))

(defn split-at-whitespace [line]
  (-> line
      (str/trim)
      (str/split #"\s+")))


