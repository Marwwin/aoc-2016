(ns aoc-2016.days.day5)
(import java.security.MessageDigest)

(declare part1 part2)

(defn solve []
  (let [p1 (part1 "ffykfhsq")
        p2 (part2 "ffykfhsq")]
    {:part1 p1 :part2 p2}))

(defn part1 [input]
  (let [algorithm (MessageDigest/getInstance "MD5")]
    (loop [i 0
           result ""]
      (if (= (count result) 8)
        result
        (let [hash-result (format "%032x" (BigInteger. 1 (.digest algorithm (.getBytes (str input i)))))
              next-ch (if (.startsWith hash-result "00000") (str result (nth hash-result 5)) result)]
          (recur (inc i) next-ch))))))

(defn part2 [input]
  (let [algorithm (MessageDigest/getInstance "MD5")]
    (loop [i 0
           res {\0 false \1 false \2 false \3 false \4 false \5 false \6 false \7 false}]
      (if (every? identity (vals res))
        (apply str (vals res))
        (let [hash-result (format "%032x" (BigInteger. 1 (.digest algorithm (.getBytes (str input i)))))
              pos (when (.startsWith hash-result "00000") (nth hash-result 5))
              new-res (if
                       (and pos (false? (res pos)))
                        (assoc res pos (nth hash-result 6))
                        res)]
          (recur (inc i) new-res))))))

(time (solve))
