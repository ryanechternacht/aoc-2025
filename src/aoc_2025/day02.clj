(ns aoc-2025.day02 
  (:require
   [clojure.string :as str]))

(defn parse-input [file]
  (->> file
       slurp
       (#(str/split % #","))
       (map #(str/split % #"-"))
       (map (partial map parse-long))))

(defn generate-from-ranges [ranges]
  (mapcat #(range (first %) (inc (second %))) ranges))

(defn is-repeated? [num]
  (let [s (str num)
        c (count s)]
    (when (even? c)
      (let [front (subs s 0 (/ c 2))
            back (subs s (/ c 2))]
        (= front back)))))

(defn solve [file]
  (->> file
       parse-input
       generate-from-ranges
       (filter is-repeated?)
       (reduce +)))

(defn is-repeated-2? [num]
  (let [s (str num)
        c (count s)]
    (loop [n 1]
      (if (> n (/ c 2)) 
        false
        (let [ps (partition n n "." s)]
          (if (apply = ps)
            true
            (recur (inc n))))))))

;; cursor suggested simplification
(defn is-repeated-2? [num]
  (let [s (str num)
        c (count s)]
    (some #(and (zero? (mod c %))
                (apply = (partition % s)))
          (range 1 (inc (quot c 2))))))

(defn solve-2 [file]
  (->> file
       parse-input
       generate-from-ranges
       (filter is-repeated-2?)
       (reduce +)))

(comment
  (parse-input "resources/day02-sample.txt")
  (is-repeated? 1010)
  (is-repeated? 999)
  (is-repeated? 1011)

  (solve "resources/day02-sample.txt")
  (solve "resources/day02.txt")

  (is-repeated-2? 123123123)
  (solve-2 "resources/day02-sample.txt")
  (solve-2 "resources/day02.txt")
  ;
  )

