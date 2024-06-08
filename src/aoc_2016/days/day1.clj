(ns aoc-2016.days.day1
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [aoc-2016.aoc-utils :as utils]))

(declare part1 part2 new-dir move manhattan)

(defn solve []
  (let [input-string (->
                      (slurp "./src/aoc_2016/days/day1_input.txt")
                      (str/trim)
                      (str/split #", "))
        instructions    (map (fn [s] [(subs s 0 1) (subs s 1)]) input-string)
        p1 (part1 instructions {:x 0 :y 0 :direction "up"})
        p2 (part2 instructions {:x 0 :y 0 :direction "up"} #{})]
    {:part1 p1 :part2 p2}))

(defn part1 [[instruction & remaining] pos]
  (if (nil? instruction)
    (utils/manhattan-2d pos {:x 0 :y 0})
    (recur remaining (move instruction pos))))

(defn inclusive-range [start end]
  (if (< start end)
    (range start  end)
    (range start  end -1)))

(defn- add-visited [pos new-pos]
  (let [news (cond
               (= (pos :x) (new-pos :x)) (map (fn [y] [(pos :x) y]) (inclusive-range (pos :y) (new-pos :y)))
               (= (pos :y) (new-pos :y)) (map (fn [x] [x (pos :y)]) (inclusive-range (pos :x) (new-pos :x))))]
    (set news)))

(def not-empty? (complement empty?))

(defn part2 [[instruction & remaining] pos visited]
  (let [new-pos (move instruction pos)
        new-visited (add-visited pos new-pos)
        intersection (set/intersection new-visited visited)]
    (if (not-empty? intersection)
      (let [v (first intersection)
            m {:x (first v) :y (second v)}]
        (utils/manhattan-2d m {:x 0 :y 0}))
      (recur remaining new-pos (set/union new-visited visited)))))

(defn move [[turn amount] {:keys [x y direction]}]
  (let [dir (new-dir turn direction)
        n (Integer/parseInt amount)]
    (case dir
      "up" {:x (+ x n) :y y :direction dir}
      "down" {:x (- x n) :y y :direction dir}
      "left" {:x x :y (- y n) :direction dir}
      "right" {:x x  :y (+ y n) :direction dir})))

(defn new-dir [turn dir]
  (case turn
    "L" (case dir
          "up" "left"
          "left" "down"
          "down" "right"
          "right" "up")
    "R" (case dir
          "up" "right"
          "right" "down"
          "down" "left"
          "left" "up")))

(solve)
