(ns aoc-2016.days.day7
  (:require [clojure.string :as str]
            [aoc-2016.aoc-utils :as utils]))

(declare is-abba split-net)

(defn solve []
  (let [input (utils/load-input "day7")
        ; the real answer is 105 but this gives 107?
        nets (map split-net input)
        p1 (filter is-abba input)]
    {:part1 (count p1)}))

(defn split-net
  [line]
  (let [nets (str/split line #"\[|\]")]
    {:hypernet  (utils/get-odds nets) :supernet (utils/get-evens nets)}))

(split-net "abc[asfwer]test[tetete]sdfsdf")

(defn is-abba [l]
  (loop [line (seq l)
         is-hypernet false]
    (if (= (count line) 3)
      false
      (let [word (take 4 line)
            is-hypernet (if (false? is-hypernet)
                          (if (= (first line) \[) true false)
                          (if (= (first line) \]) false true))]

        (if (and
             (not= (first word) (second word))
             (= (first word) (nth word 3))
             (= (second word) (nth word 2)))
          (if (false? is-hypernet)
            true
            false)
          (recur (rest line) is-hypernet))))))

(solve)
