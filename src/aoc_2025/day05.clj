(ns aoc-2025.day05
  (:require [clojure.string :as str]))

(defn read-file [file]
  (let [lines (->> file
                   slurp
                   str/split-lines)
        ranges (->> lines
                    (take-while seq)
                    (map #(str/split % #"-"))
                    (map #(map parse-long %)))
        in-ranges? #(some (fn [[a b]] (<= a % b)) ranges)
        tests (->> lines
                   (drop-while seq)
                   rest ;; skip blank line
                   (map parse-long))]
    {:tests tests
     :ranges ranges
     :in-ranges? in-ranges?}))

(defn solve-1 [file]
  (let [{:keys [tests in-ranges?]}
        (read-file file)]
    (->> tests
         (filter in-ranges?)
         count)))

(defn overlap? [[a1 a2] [b1 b2]]
  (or (<= a1 b1 a2)
      (<= a1 b2 a2)
      (<= b1 a1 b2)
      (<= b1 a2 b2)))

(comment
  (overlap? [0 5] [3 8])
  (overlap? [0 5] [6 8])
  (overlap? [0 5] [1 3])
  (overlap? [0 5] [0 5])
  (overlap? [0 5] [-1 3])
  (overlap? [0 5] [-1 6])
  ;
  )

(defn combine [[a1 a2] [b1 b2]]
  [(min a1 b1) (max a2 b2)])

(comment
  (combine [0 5] [3 8])
  (combine [0 5] [1 3])
  (combine [0 5] [0 5])
  (combine [0 5] [-1 3])
  (combine [0 5] [-1 6])
  ;
  )

(defn solve-2 [file]
  (let [{start :ranges} (read-file file)
        final-ranges
        (loop [ranges (set start)
               [r & rs] (set start)]
          (if (nil? r)
            ranges
            (if-let [r2 (some #(when (overlap? r %) %)
                              (disj ranges r))]
              (let [combined (combine r r2)]
                (recur (-> ranges
                           (disj r)
                           (disj r2)
                           (conj combined))
                       (-> rs
                           set
                           (disj r2)
                           (conj combined))))
              (recur ranges (set rs)))))]
    (->> final-ranges 
         (map (fn [[a b]] (- (inc b) a)))
         (reduce +))))

(comment
  (let [{:keys [tests in-ranges?]}
        (read-file "resources/day05-sample.txt")]
    (in-ranges? 3))

  (solve-1 "resources/day05-sample.txt")
  (solve-1 "resources/day05.txt")

  (solve-2 "resources/day05-sample.txt")
  (solve-2 "resources/day05.txt")
  ;
  )
