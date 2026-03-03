(ns aoc-2025.day06
  (:require [clojure.string :as str]))

(defn read-file [file]
  (->> file
       slurp
       str/split-lines
       (map #(re-seq #"\S+" %))
       (apply map list)))

(def op-map {"*" * "+" +})

(defn process-list [l]
  (let [nums (map parse-long (butlast l))
        op (op-map (last l))]
    (apply op nums)))

(defn solve-1 [file]
  (->> file
       read-file
       (map process-list)
       (reduce +)))

(defn read-file-2 [file]
  (let  [lines (->> file
                    slurp
                    str/split-lines)
         ops (->> lines
                  last
                  (#(re-seq #"\S+" %))
                  (map op-map))
         nums (->> lines
                   butlast
                   (apply map str)
                   (map str/trim)
                   (map parse-long))
         groups (->> nums
                     (partition-by nil?)
                     (remove #(every? nil? %)))]
    {:ops ops
     :nums nums
     :groups groups}))

(defn solve-2 [file]
  (let [{:keys [ops groups]} (read-file-2 file)]
    (reduce + (map apply ops groups))))

(comment
  (read-file "resources/day06-sample.txt")

  (solve-1 "resources/day06-sample.txt")
  (solve-1 "resources/day06.txt")

  (read-file-2 "resources/day06-sample.txt")
  (read-file-2 "resources/day06.txt")
  (solve-2 "resources/day06-sample.txt")
  (solve-2 "resources/day06.txt")
  ;
  )
