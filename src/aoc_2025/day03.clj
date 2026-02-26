(ns aoc-2025.day03 
  (:require
   [clojure.string :as str]))

(defn read-input [file]
  (->> file
       slurp
       (str/split-lines)
       (map (fn [s] (map #(Character/digit % 10) s)))))

(defn find-largest [nums]
  (let [largest (reduce max nums)]
    (->> nums
         (keep-indexed #(when (= largest %2) [%1 %2]))
         first)))

(defn find-largest-joltage [nums]
  (let [[i1 n1] (find-largest (butlast nums))
        [_ n2] (find-largest (drop (inc i1) nums))]
    (parse-long (str n1 n2))))

(defn solve [file]
  (->> file
       read-input
       (map find-largest-joltage)
       (reduce +)))

(defn find-largest-joltage-2 [turn-on nums]
  (let [output (loop [output []
                      start-from 0]
                 (if (= turn-on (count output))
                   output
                   (let [[i n] (find-largest
                                (->> nums
                                     (drop start-from)
                                     (drop-last (- turn-on (count output) 1))))]
                     (recur (conj output n) (+ start-from (inc i))))))]
    (->> output (apply str) parse-long)))

(defn solve-2 [file]
  (->> file
       read-input
       (map (partial find-largest-joltage-2 12))
       (reduce +)))

(comment
  (solve "resources/day03-sample.txt")
  (solve "resources/day03.txt")

  (find-largest '(8 1 1 1 1 1 1 1 1 1 1 1 1 1 9))
  (find-largest-joltage '(8 1 1 1 1 1 1 1 1 1 1 1 1 1 9))

  (find-largest-joltage-2 2 '(8 1 1 1 1 1 1 1 1 1 1 1 1 1 9))
  (find-largest-joltage-2 12 '(8 1 1 1 1 1 1 1 1 1 1 1 1 1 9))

  (find-largest-joltage-2 4 '(9 8 7 6 5 4 3 2 1 1 1 1 1 1 1))

  (solve-2 "resources/day03-sample.txt")
  (solve-2 "resources/day03.txt")
  ;
  )