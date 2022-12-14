(ns day4)

(require '[clojure.string :as str])

(defn complete-overlap [[low-1 high-1 low-2 high-2]]
  (or
   (and (<= low-1 low-2) (>= high-1 high-2))
   (and (<= low-2 low-1) (>= high-2 high-1))))

(defn any-overlap [[low-1 high-1 low-2 high-2]]
  (and (<= low-1 high-2) (<= low-2 high-1)))

(defn parse-input [assignment]
  (->>
   (str/split assignment #",")
   (map #(str/split % #"\-"))
   (flatten)
   (map read-string)))

(defn part-1 [input] 
  (println 
   (->>
    (str/split input #"\n")
    (map parse-input)
    (filter complete-overlap)
    (count))))

(defn part-2 [input]
  (println
   (->>
    (str/split input #"\n")
    (map parse-input)
    (filter any-overlap)
    (count))))

(defn start [] ((juxt part-1 part-2) (slurp "inputs/day4")))

(start)
