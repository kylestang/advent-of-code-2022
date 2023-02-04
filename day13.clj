(ns day13)

(require '[clojure.string :as str])

(declare compare-values)

(defn handle-collection [left right]
  (let [result (compare-values (first left) (first right))]
    (if (and (nil? result) (coll? left) (coll? right))
      (handle-collection (next left) (next right))
      result)))

(defn compare-values [left right]
  (cond
    (and (nil? left) (nil? right)) nil
    (nil? left) true
    (nil? right) false
    (and (coll? left) (coll? right)) (handle-collection left right)
    (coll? left) (compare-values left [right])
    (coll? right) (compare-values [left] right)
    (< left right) true
    (> left right) false
    :else nil))

(defn count-ordered [index pairs]
  (let [[left right] (map read-string (str/split pairs #"\n"))]
    (if (compare-values left right)
      (inc index)
      0)))

(defn count-smaller [val list]
  (count (filter true? (map #(compare-values % val) list))))

(defn part-1 [input]
  (println
   (reduce + (map-indexed count-ordered (str/split
    (str/replace input #"," " ") #"\n\n")))))

(defn part-2 [input]
  (println
   (let
    [data
     (map
      read-string
      (str/split
       (str/replace (str/replace input #"," " ") #"\n\n" "\n") #"\n"))]
     (*
      (inc (count-smaller [[2]] data))
      (+ (count-smaller [[6]] data) 2)))))

(part-1 (slurp "inputs/day13"))
(part-2 (slurp "inputs/day13"))
