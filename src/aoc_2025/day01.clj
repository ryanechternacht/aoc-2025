(ns aoc-2025.day01
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-input [file]
  (->> file
       (slurp)
       (str/split-lines)))

(defn parse-line [line]
  (let [[_ direction distance] (re-find #"([LR])(\d+)" line)
        direction-multiplier (if (= direction "L") -1 1)]
    (* direction-multiplier (Integer/parseInt distance))))

(defn rotate [[pos zeros] adj]
  (let [new-pos (mod (+ pos adj) 100)]
    [new-pos (cond-> zeros (zero? new-pos) inc)]))

(defn solve [file]
  (->> file
       read-input
       (map parse-line)
       (reduce rotate [50 0])
       second))

(defn rotate-2 [[pos zeros] adj]
  (let [new-pos (+ pos adj)
        crosses (quot new-pos 100)
        crosses-2 (cond-> (abs crosses)
                    ;; (pos? pos) handles edge case of starting at 0 and going negative which was counting extra crosses
                    (and (neg? new-pos) (pos? pos)) inc
                    ;; handles landing on zero from a leftward turn
                    (zero? new-pos) inc)]
    #_(println pos adj new-pos crosses crosses-2 (neg? new-pos) (pos? pos) [(mod new-pos 100) (+ zeros crosses-2)])
    [(mod new-pos 100) (+ zeros crosses-2)]))

(defn solve-2 [file]
  (->> file
       read-input
       (map parse-line)
       (reduce rotate-2 [50 0])
       second))

(comment
  (solve "resources/day01-sample.txt")
  (solve "resources/day01.txt")
  (solve-2 "resources/day01-sample.txt")
  (solve-2 "resources/day01.txt")

  (rotate-2 [50 0] 50)
  (rotate-2 [50 0] -50)
  (rotate-2 [50 0] -150)

  ;; Basic forward rotations
(rotate-2 [99 0] 5)      ; 99 → 0 → 1 → 2 → 3 → 4 (crosses 0 once)
(rotate-2 [50 0] 50)     ; 50 → 51 → ... → 99 → 0 (crosses 0 once)
(rotate-2 [50 0] 150)     ; 50 → ... → 99 → 0 → ... → 0 (crosses 0 twice)
(rotate-2 [1 0] 99)       ; 1 → 2 → ... → 99 → 0 (crosses 0 once)

;; Basic backward rotations
(rotate-2 [1 0] -5)       ; 1 → 0 → 99 → 98 → 97 → 96 (crosses 0 once)
(rotate-2 [50 0] -50)     ; 50 → 49 → ... → 1 → 0 (crosses 0 once)
(rotate-2 [50 0] -150)    ; 50 → ... → 1 → 0 → 99 → ... → 0 (crosses 0 twice)

;; Starting at 0 cases
(rotate-2 [0 0] 5)       ; 0 → 1 → 2 → 3 → 4 → 5 (crosses 0 once - when you leave 0)
(rotate-2 [0 0] 100)     ; 0 → 1 → ... → 99 → 0 (crosses 0 once)
(rotate-2 [0 0] 200)     ; 0 → ... → 0 → ... → 0 (crosses 0 twice)
(rotate-2 [0 0] -5)       ; 0 → 99 → 98 → 95 (does NOT cross 0 - you're already there)
(rotate-2 [0 0] -100)     ; 0 → 99 → ... → 0 (crosses 0 once - wraps back to 0)

;; Edge cases with quot values
(rotate-2 [15 0] -15)     ; 15 → ... → 1 → 0 (crosses 0 once, quot = 0, new-pos = 0)
(rotate-2 [15 0] -16)     ; 15 → ... → 0 → 99 (crosses 0 once, quot = 0, new-pos = -1)
(rotate-2 [15 0] -200)    ; 15 → ... → 0 → ... → 15 (crosses 0 twice, quot = -2)

;; Multiple wraps
(rotate-2 [50 0] 250)     ; Should cross 0 twice (50 → ... → 0 → ... → 0 → 50)
(rotate-2 [50 0] -250)    ; Should cross 0 twice (50 → ... → 0 → ... → 0 → 50)
  ;
  )
