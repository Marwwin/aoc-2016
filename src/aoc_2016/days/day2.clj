(ns aoc-2016.days.day2
  (:require [clojure.string :as str]))

(declare walk  move)

(defn solve []
  (let [data (->
              (slurp "./src/aoc_2016/days/day2_input.txt")
              (str/split-lines))]
    (walk (map seq data) {:x 1 :y 1} {:width 2 :height 2} ""))) ; rewrite the pad to be a nested vector access with get-in

(defn walk [[f & remaining] pos keypad result]
  (if (nil? f)
    result
    (let [p (move pos f keypad)]
      (println p)
      (recur remaining p keypad (str result "")))))

(defn move [pos [instr & remaining] pad]
  (if (nil? instr)
    pos
    (let [n-p (case instr
                \L {:x (max 0 (dec (pos :x))) :y (pos :y)}
                \R {:x (min (pad :width) (inc (pos :x))) :y (pos :y)}
                \U {:x (pos :x) :y (max 0 (dec (pos :y)))}
                \D {:x (pos :x) :y (min (pad :height) (inc (pos :y)))})]
      (recur n-p remaining pad))))

(solve)
