(ns aoc-2025.day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-file [file]
  (->> file
       slurp
       str/split-lines
       (map (fn [s]
              (map parse-long (str/split s #","))))))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                (Math/pow (- y1 y2) 2)
                (Math/pow (- z1 z2) 2))))

(defn compare-points [[a1 b1 c1] [a2 b2 c2]]
  (let [x (compare a1 a2)
        y (compare b1 b2)]
    (cond
      (not= 0 x) x
      (not= 0 y) y
      :else (compare c1 c2))))

(defn generate-sorted-unique-pairs-with-distance [points]
  (sort-by last
           (for [a points
                 b points
                 :when (neg? (compare-points a b))]
             [a b (distance a b)])))

(defn create-junction [{:keys [connections max-connections junctions] :as acc} [a b]]
  (let [a-match (some #(when (% a) %) junctions)
        b-match (some #(when (% b) %) junctions)]
   (cond
      (= (inc connections) max-connections)
      (reduced junctions)

      (and (some? a-match) (some? b-match))
      (-> acc
          (update :connections inc)
          (update :junctions disj a-match)
          (update :junctions disj b-match)
          (update :junctions conj (set/union b-match a-match)))

      (some? a-match)
      (-> acc
          (update :connections inc)
          (update :junctions disj a-match)
          (update :junctions conj (conj a-match b)))

      (some? b-match)
      (-> acc
          (update :connections inc)
          (update :junctions disj b-match)
          (update :junctions conj (conj b-match a)))

      :else
      (-> acc
          (update :connections inc)
          (update :junctions conj #{a b})))))

(defn create-junctions [max-connections pairs]
  (reduce
   create-junction
   {:connections 0 :max-connections max-connections :junctions #{}}
   pairs))

(defn solve [file connections]
  (->> file
       read-file
       generate-sorted-unique-pairs-with-distance
       (create-junctions connections)
       (map count)
       (sort >)
       (take 3)
       (reduce *)))

(defn create-junction-2 [{:keys [target-size junctions] :as acc} [a b]]
  (let [a-match (some #(when (% a) %) junctions)
        b-match (some #(when (% b) %) junctions)]
   (cond
      (= target-size (count (conj (set/union a-match b-match) a b)))
      (reduced [a b])
  
      (and (some? a-match) (= a-match b-match))
      acc
  
      (and (some? a-match) (some? b-match))
      (-> acc
          (update :junctions disj a-match)
          (update :junctions disj b-match)
          (update :junctions conj (set/union b-match a-match)))
  
      (some? a-match)
      (-> acc
          (update :junctions disj a-match)
          (update :junctions conj (conj a-match b)))
  
      (some? b-match)
      (-> acc
          (update :junctions disj b-match)
          (update :junctions conj (conj b-match a)))
  
      :else
      (-> acc
          (update :junctions conj #{a b})))))

(defn solve-2 [file target-size]
  (->> file
       read-file
       generate-sorted-unique-pairs-with-distance
       (reduce create-junction-2
               {:target-size target-size :junctions #{}})
       (map first)
       (reduce *)))

(comment
  (read-file "resources/day08-sample.txt")

  (distance '(162 817 812) '(425 690 689))
  (distance '(984,92,344) '(805,96,715))

  (create-junction {:connections 0
                    :max-connections 10
                    :junctions #{}}
                   ['(0 0 0) '(1 1 1) 1])

  (create-junction {:connections 1
                    :max-connections 10
                    :junctions #{#{'(0 0 0) '(2 2 2)}
                                 #{'(3 3 3)}}}
                   ['(0 0 0) '(1 1 1) 1])

  (create-junction {:connections 2
                    :max-connections 10
                    :junctions #{#{'(1 1 1) '(2 2 2)}
                                 #{'(3 3 3)}}}
                   ['(0 0 0) '(1 1 1) 1])

  (create-junction {:connections 3
                    :max-connections 10
                    :junctions #{#{'(1 1 1) '(2 2 2)}
                                 #{'(0 0 0) '(3 3 3)}
                                 #{'(4 4 4)}}}
                   ['(0 0 0) '(1 1 1) 1])

  (create-junction {:connections 4
                    :max-connections 10
                    :junctions #{#{'(1 1 1) '(2 2 2) '(5 5 5)}
                                 #{'(0 0 0) '(3 3 3)}
                                 #{'(4 4 4)}}}
                   ['(1 1 1) '(5 5 5) 1])

  (create-junction {:connections 9
                    :max-connections 10
                    :junctions #{#{'(1 1 1) '(2 2 2)}
                                 #{'(0 0 0) '(3 3 3)}
                                 #{'(4 4 4)}}}
                   ['(0 0 0) '(1 1 1) 1])

  (solve "resources/day08-sample.txt" 11)
  (solve "resources/day08.txt" 1000)

  (solve-2 "resources/day08-sample.txt" 20)
  (solve-2 "resources/day08.txt" 1000)
  ;
  )
