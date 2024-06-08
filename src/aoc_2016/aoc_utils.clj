(ns aoc-2016.aoc-utils)

(defn manhattan-2d
  [vec1  vec2]
  (+
   (Math/abs (- (vec2 :x) (vec1 :x)))
   (Math/abs (- (vec2 :y) (vec1 :y)))))
