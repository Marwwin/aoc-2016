(ns aoc-2016.days.day4
  (:require [clojure.string :as string])
  (:require [aoc-2016.aoc-utils :as utils]))

(declare parse-line is-real-room north-p shift-char)

(def low-limit (int \a))
(def high-limit (inc (int \z)))

(defn solution []
  (let [input (->>
               (utils/load-input "day4")
               (map parse-line))
        p1 (->>
            input
            (filter is-real-room)
            (map :id)
            (apply +))
        p2 (->>
            input
            (map north-p)
            (filter (fn [room] (string/includes? (room :name) "north")))
            )]

    {:part1 p1 :part2 p2}))

(defn word-magic [word m]
  (map
   (fn [ch] (char (let [ch-val (int ch)
                        new-val (+ ch-val m)]
                    (if (> new-val high-limit)
                      (+ low-limit (mod new-val high-limit))
                      new-val))))
   word))

(defn north-p [{:keys [name id]}]
  (let [m (mod id 26)
        b (string/join " " (map (fn [word] (apply str (word-magic word m))) name))]
    {:name  b :id id}))

(defn is-real-room [{:keys [name checksum]}]
  (let [freq (frequencies (string/join name))
        most-common (take 5 (sort-by (juxt (comp - val) key) freq))
        to-test (string/join (map first most-common))]
    (= to-test checksum)))

(defn parse-line [line]
  (let [splitted (string/split line #"-")
        name (drop-last splitted)
        id-and-checksum (string/split (last splitted) #"\[")
        id (Integer/parseInt (first id-and-checksum))
        checksum (let [temp-checksum (last id-and-checksum)]
                   (subs temp-checksum 0 (dec (count temp-checksum))))]
    {:name name :id id :checksum checksum}))

(solution)
