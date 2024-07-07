(ns aoc-2016.days.day8
  (:require [clojure.string :as str]
            [aoc-2016.aoc-utils :as utils]))

(declare part1 new-screen print-screen update-screen rect rotate get-row get-col shift-vec count-lit)

(defn solve []
  (let [input (utils/load-input "day8")
        screen (new-screen 50 6)
        res (part1 input screen)]
    (print-screen res)
    {:part1 (count-lit res) :part2 "see above"}))

(defn part1 [instructions screen]
  (loop [instructions instructions
         screen screen]
    (if (empty? instructions)
      screen
      (let [[instruction & args] (str/split (first instructions)  #"\s+")]
        (recur (rest instructions) (case  instruction
                                     "rect" (rect screen args)
                                     "rotate" (rotate screen args)))))))

(defn rect [screen [size]]
  (let [[x y] (str/split size #"x")
        ys (range (Integer/parseInt y))
        xs (range (Integer/parseInt x))
        width (screen :width)
        to-add (->> ys (mapcat (fn [y-index]
                                 (->> xs
                                      (map
                                       (fn [x-index]
                                         [(+ x-index (* y-index width)) "x"]))))))
        newp (update-screen screen to-add)]
    (assoc screen :pixels newp)))

(defn update-screen
  [screen to-add]
  (reduce
   (fn [acc [idx ch]]
     (assoc acc idx ch))
   (screen :pixels)
   to-add))

(defn rotate [screen [row-col i _ n]]
  (let [i (Integer/parseInt (second (str/split i #"=")))
        n (Integer/parseInt n)]
    (assoc
     screen :pixels
     (let [to-change (case row-col
                       "row"  (get-row screen i n)
                       "column"  (get-col screen i n))]
       (update-screen screen to-change)))))

(defn get-col [screen i n]
  (let  [symbols (mapcat (fn [coll] [(nth coll i)])
                         (partition (screen :width) (screen :pixels)))
         indexes (map (fn [ii]  (+ i (* ii (screen :width))))
                      (range (screen :height)))]
    (map vector (shift-vec indexes n) symbols)))
(get-col (rect (new-screen 5 5) ["3x2"])  1 1)

(defn shift-vec [v n]
  (let [v (vec v)]
    (vec (concat
          (subvec v n)
          (subvec v 0 n)))))

(defn get-row [screen i n]
  (let [start (* i (screen :width))
        end (* (inc i) (screen :width))]
    (map vector
         (shift-vec (range start end) n)
         (subvec (screen :pixels) start end))))

(defn new-screen [wide high]
  {:pixels (vec (repeat (* wide high) "."))
   :width wide
   :height high})

(defn print-screen [screen]
  (let [s (partition (screen :width) (screen :pixels))]
    (doseq [row s]
      (println row))))

(defn count-lit [screen]
  (count (filter (fn [c] (= c "x")) (screen :pixels))))

(solve)
