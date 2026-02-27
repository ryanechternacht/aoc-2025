(ns aoc-2025.day04
  (:require [clojure.string :as str]))

(defn read-lines [file]
  (->> file
       slurp
       (str/split-lines)))

(defn build-map [file]
  (->> file
       read-lines
       (map-indexed
        (fn [y line]
          (map-indexed #(vec [%1 y %2]) line)))
       (mapcat identity)
       (filter #(= (nth % 2) \@))
       (map (fn [[x y _]] [x y]))
       set))

(def adjancies #{[-1 -1] [0 -1] [1 -1]
                 [-1 0] [1 0]
                 [-1 1] [0 1] [1 1]})
(defn count-adjancies [m [x y]]
  (->> adjancies
       (map (fn [[x2 y2]] [(+ x x2) (+ y y2)]))
       (map m)
       (filter identity)
       count))

(defn solve [file]
  (let [m (build-map file)]
    (->> m
         (map #(count-adjancies m %))
         (filter #(< % 4))
         count)))

(defn solve-2 [file]
  (let [start (build-map file)
        final (loop [m start]
                (println (count m))
                (let [pred #(< (count-adjancies m %) 4)]
                  (if-let [r (some #(when (pred %) %) m)]
                    (recur (disj m r))
                    m)))]
    (- (count start) (count final))))

(comment
  (build-map "resources/day04-sample.txt")

  (def m (build-map "resources/day04-sample.txt"))
  (count-adjancies m [1 0])

  (solve "resources/day04-sample.txt")
  (solve "resources/day04.txt")

  (solve-2 "resources/day04-sample.txt")
  (solve-2 "resources/day04.txt")
  ;
  )
