(ns aoc-2025.day07
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [s]
  (->> s
       (keep-indexed #(when (= \^ %2) %1))
       (into #{})))

(defn read-file [file]
  (let [lines (->> file
                   slurp
                   str/split-lines)
        beam (-> lines
                 first
                 (.indexOf "S"))
        steps (->> lines
                   (drop 1)
                   (map parse-line))]
    {:beams (conj #{} beam)
     :steps steps}))

(defn step [{:keys [beams split-count]} splitters]
  (let [splits (set/intersection beams splitters)
        new-beams (->> splits
                       (map #(vector (inc %) (dec %)))
                       flatten
                       (into #{}))]
    {:beams (-> beams
                (set/difference splits)
                (set/union new-beams))
     :split-count (+ split-count (count splits))}))

(defn solve [file]
  (let [{:keys [beams steps]} (read-file file)]
    (:split-count (reduce step {:beams beams :split-count 0} steps))))

(defn safe+ [& xs]
  (reduce + (map #(if (nil? %) 0 %) xs)))

(defn split-beam [beams split]
  (let [c (beams split)]
    (-> beams
        (dissoc split)
        (update (inc split) safe+ c)
        (update (dec split) safe+ c))))

(defn step-2 [beams splitters]
  (let [splits (set/intersection (set (keys beams)) splitters)]
    (reduce split-beam beams splits)))

(defn solve-2 [file]
  (let [{:keys [beams steps]} (read-file file)
        beam-map (->> beams
                      (map (fn [x] {x 1}))
                      (into {}))
        final (reduce step-2 beam-map steps)]
    (reduce + (vals final))))

(comment
  (read-file "resources/day07-sample.txt")

  (step {:beams #{1 3 5 9} :split-count 4} #{2 3 5})

  (solve "resources/day07-sample.txt")
  (solve "resources/day07.txt")

  (safe+ nil nil 2 3 nil nil)

  (split-beam {1 4 2 3} 1)

  (step-2 {1 4
           2 3
           3 4
           4 5
           5 6}
          #{1 3})

  (solve-2 "resources/day07-sample.txt")
  (solve-2 "resources/day07.txt")
  ;
  )
