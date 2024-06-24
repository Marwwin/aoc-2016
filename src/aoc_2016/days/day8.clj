(ns aoc-2016.days.day8
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [aoc-2016.aoc-utils :as utils]))

(declare part1 new-screen print-screen set-pixel rect screen-at rotate get-row get-col shift index-to-pos)

(defn solve []
  (let [input (utils/load-input "day8")
        screen (new-screen 50 6)]
    (part1 input screen)))

(defn part1 [inst screen]
  (loop [instrs inst
         screen screen]
    (println "recur" (first instrs))
    (if (empty? instrs)
      (print-screen screen)
      (let [instruction (str/split (first instrs)  #"\s+")
            scr (case (first instruction)
                  "rect" (rect screen (rest instruction))
                  "rotate" (rotate screen (rest instruction)))]
        (println "hepp" instruction)
        (recur (rest instrs) scr)))))

(defn rect [screen [instr]]
  (println "h" (screen :pixels))
  (let [[x y] (str/split instr #"x")
        to-add (flatten (map (fn [row-i]
                               (map (fn [i] (+ i (* row-i (screen :width))))
                                    (range (Integer/parseInt x))))
                             (range (Integer/parseInt y))))
        newp (reduce (fn [acc idx]
                       (assoc acc idx "x"))
                     (screen :pixels)
                     to-add)]
    (println newp)
    (assoc screen :pixels newp)))

(defn rotate [screen [row-col i _ n]]
  (println i n)
  (let [i (Integer/parseInt (second (str/split i #"=")))]
    (assoc
     screen :pixels
     (case row-col
       "row" (shift (get-row screen i) n)
       "column" (shift (get-col screen i) n)))))

; This needs to update the correct row-col in pixels 
(defn shift [coll n]
  (vec (concat (drop n coll) (take n coll))))

(defn get-col [screen i]
  (map
   (fn [coll] (nth coll i))
   (partition (screen :width) (screen :pixels))))

(defn get-row [screen i]
  (subvec
   (screen :pixels)
   (* i (screen :width))
   (* (inc i) (screen :width))))

(defn new-screen [wide high]
  {:pixels (vec (repeat (* wide high) "."))
   :width wide
   :height high})

;(defn set-pixel [screen pos value]
;  (let [new-pixels (assoc (vec (screen :pixels)) (screen-at screen pos) value)]
;    (assoc screen :pixels new-pixels)))
;
;(set-pixel (new-screen 5 5) {:x 0 :y 0} "x")

;(defn index-to-pos [screen cnt]
;  (if (or (>= cnt (screen :pixels)) (< cnt 0))
;    (throw
;     (ex-info (str "Value outside of screen " cnt) {:type :out-of-bound}))
;    (let [x (mod cnt (screen :width))
;          y (->> (/ cnt (screen :width))
;                 math/floor
;                 int)]
;      {:x x :y y})))
;
;(defn screen-at [screen pos]
;  (let [x (pos :x)
;        y (pos :y)]
;    (if (or (< x 0) (>= x (screen :width))
;            (< y 0) (>= y (screen :height)))
;      (throw
;       (ex-info
;        (str "Position outside of screen " pos)
;        {:type :out-of-bound}))
;      (+
;       (* y (screen :width)) x))))

(defn print-screen [screen]
  (let [s (partition (screen :width) (screen :pixels))]
    (doseq [row s]
      (println row))))

(solve)
