(ns aoc-2016.days.day2
  (:require [clojure.string :as str])
  (:require [aoc-2016.aoc-utils :as utils]))

(declare walk move)

(def part1-keypad [[1 2 3]
                   [4 5 6]
                   [7 8 9]])

(def part2-keypad [[nil nil  1  nil  nil]
                   [nil  2   3   4   nil]
                   [5    6   7   8    9]
                   [nil \A  \B  \C   nil]
                   [nil nil \D  nil  nil]])

(defn solve []
  (let [data (utils/load-input "day2")
        p1 (walk (map seq data) {:x 1 :y 1} part1-keypad  "")
        p2 (walk (map seq data) {:x 0 :y 2} part2-keypad  "")]
    {:part1 p1 :part2 p2}))

(defn walk [[instruction & remaining] position keypad result]
  (if (nil? instruction)
    result
    (let [new-position (move position instruction keypad)
          button (get-in keypad [(new-position :y) (new-position :x)])]
      (recur remaining new-position keypad (str result button)))))

(defn move [position [instr & remaining] keypad]
  (if (nil? instr)
    position
    (let [temp-position
          (case instr
            \L {:x (dec (position :x)) :y (position :y)}
            \R {:x (inc (position :x)) :y (position :y)}
            \U {:x (position :x) :y (dec (position :y))}
            \D {:x (position :x) :y (inc (position :y))})
          new-position (if (nil? (get-in
                                  keypad
                                  [(temp-position :y) (temp-position :x)]))
                         position
                         temp-position)]
      (recur new-position remaining keypad))))

(solve)
