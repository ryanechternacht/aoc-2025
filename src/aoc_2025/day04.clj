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
                (let [pred #(< (count-adjancies m %) 4)]
                  (if-let [r (some #(when (pred %) %) m)]
                    (recur (disj m r))
                    m)))]
    (- (count start) (count final))))

;; runtime improvement for step 2
(defn remove-pass [m]
  (->> m
       (keep #(when (< 3 (count-adjancies m %)) %))
       set))

(defn faster-solve [file]
  (let [start (build-map file)
        final (loop [m start
                     last-count (count start)]
                (let [new-m (remove-pass m)]
                  (if (= last-count (count new-m))
                    m
                    (recur new-m (count new-m)))))]
    (- (count start) (count final))))

;; a faster approach would be to put all of these in a queue, 
;; then whenever we remove one, to re-add it's neighbors to that queue
;; for re-evaluation. after a few runs this should limit the amount
;; of re-evaluation significantly. 

;; went ahead and did that, and it's much faster

(defn make-adjancies [m [x y]]
  (->> adjancies
       (map (fn [[x2 y2]] [(+ x x2) (+ y y2)]))
       (map m)
       (filter identity)))

(defn fastest-solve [file]
  (let [start (build-map file)
        final
        (loop [m start
               [p & ps] start]
          (cond
            (nil? p) m

            (< (count-adjancies m p) 4)
            (let [new-m (disj m p)
                  new-ps (into ps (make-adjancies new-m p))]
              (recur new-m new-ps))

            :else (recur m ps)))]
    (- (count start) (count final))))

(comment
  (build-map "resources/day04-sample.txt")

  (def m (build-map "resources/day04-sample.txt"))
  (count-adjancies m [1 0])
  (make-adjancies m [0 0])

  (solve "resources/day04-sample.txt")
  (solve "resources/day04.txt")

  (solve-2 "resources/day04-sample.txt")
  (solve-2 "resources/day04.txt")

  (->> "resources/day04-sample.txt"
       build-map
       remove-pass
       count)

  (faster-solve "resources/day04-sample.txt")
  (faster-solve "resources/day04.txt")

  (fastest-solve "resources/day04-sample.txt")
  (fastest-solve "resources/day04.txt")

  (time (solve-2 "resources/day04.txt"))
  ; 47502.452583 msecs
  (time (faster-solve "resources/day04.txt"))
  ; 541.654166 msecs
  (time (fastest-solve "resources/day04.txt"))
  ; 218.734792 msecs

  ;
  )
