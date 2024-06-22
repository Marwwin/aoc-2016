(ns aoc-2016.days.day8
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [aoc-2016.aoc-utils :as utils]))

(declare part1 new-screen print-screen set-pixel screen-at index-to-pos)

(defn solve []
  (let [input (utils/load-input "day8")
        screen (new-screen 50 6)]
    (part1 input screen)))

(defn part1 [inst screen]
  (loop [instrs inst
         screen screen]
    (if (empty? instrs)
      (print-screen screen)
      (let [instruction (str/split (first instrs)  #"\s+")
            scr (case (first instruction)
                  "rect" ())]
        (println instruction)
        (recur (rest instrs) scr)))))

(defn rect [screen instr]
  (let [[x y] (str/split instr #"x")
        to-add (flatten (map (fn [row-i]
                               (map (fn [i] (+ i (* row-i (screen :width))))
                                    (range (Integer/parseInt x))))
                             (range (Integer/parseInt y))))
        newp (reduce (fn [acc idx]
                       (assoc acc idx "x"))
                     (screen :pixels)
                     to-add)]
    (assoc screen :pixels newp)))

(->> (rect (new-screen 5 5) "4x2") print-screen)

(defn new-screen [wide high]
  {:pixels (vec (repeat (* wide high) "."))
   :width wide
   :height high})

(defn set-pixel [screen pos value]
  (let [new-pixels (assoc (vec (screen :pixels)) (screen-at screen pos) value)]
    (assoc screen :pixels new-pixels)))

;(set-pixel (new-screen 5 5) {:x 0 :y 0} "x")

(defn index-to-pos [screen cnt]
  (if (or (>= cnt (screen :pixels)) (< cnt 0))
    (throw
     (ex-info (str "Value outside of screen " cnt) {:type :out-of-bound}))
    (let [x (mod cnt (screen :width))
          y (->> (/ cnt (screen :width))
                 math/floor
                 int)]
      {:x x :y y})))

(defn screen-at [screen pos]
  (let [x (pos :x)
        y (pos :y)]
    (if (or (< x 0) (>= x (screen :width))
            (< y 0) (>= y (screen :height)))
      (throw
       (ex-info
        (str "Position outside of screen " pos)
        {:type :out-of-bound}))
      (+
       (* y (screen :width)) x))))

(defn print-screen [screen]
  (let [s (partition (screen :width) (screen :pixels))]
    (doseq [row s]
      (println row))))

(solve)
