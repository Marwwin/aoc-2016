(ns aoc-2016.days.day7
  (:require [clojure.string :as str]
            [aoc-2016.aoc-utils :as utils]
            [clojure.set :as set]))

(declare is-abba is-palindrome split-net supports-tls aba-to-bab find-aba-substring)

(defn solve []
  (let [input (utils/load-input "day7")
        ips (map split-net input)
        p1 (filter supports-tls ips)
        p2 (filter (fn [ip] (let [a (set (find-aba-substring (str/join "  " (ip :hypernet))))
                                  b (find-aba-substring (str/join "  " (ip :supernet)))
                                  c (set (map aba-to-bab b))]
                              (println ip a c)
                              (not-empty (set/intersection a c)))) ips)]

    {:part1 (count p1) :part2 (count p2)}))

(defn split-net [line]
  (let [sequences (str/split line #"\[|\]")]
    {:hypernet  (utils/get-odds sequences) :supernet (utils/get-evens sequences)}))

(defn supports-tls [ip]
  (if (some is-abba (ip :hypernet))
    false
    (some is-abba (ip :supernet))))

(defn is-abba [ip]
  (loop [line ip]
    (if (<= (count line) 3)
      false
      (let [word (take 4 line)]
        (if (is-palindrome word)
          true
          (recur (rest line)))))))

(defn is-palindrome [word]
  (and
   (not= (first word) (second word))
   (= (first word) (nth word 3))
   (= (second word) (nth word 2))))

(defn find-aba-substring [line]
  (->> line
       (re-seq #"(.)((?!\1)[a-z])\1")
       (map first)))

(find-aba-substring "xxxasas")

(defn aba-to-bab [word]
  (str (second word) (first word) (second word)))

(solve)
