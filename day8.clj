(ns day8)

(require '[clojure.string :as str])

(defn parse-input [tree]
  {:height (Character/digit tree 10) :visible false})

(defn mark-tree [max tree]
  {:height (tree :height) 
   :visible (or (tree :visible) (> (tree :height) max))})

(defn mark-row [row]
  ((reduce
   (fn [accum tree] 
     {:max (max (accum :max) (tree :height))
      :trees (conj (accum :trees) (mark-tree (accum :max) tree))})
   {:max -1 :trees []}
   row) :trees))

(defn count-visible [max row]
  (count
   (let [[front back] (split-with (partial > max) row)]
    (if (first back)
      (conj front (first back))
      front))))

(defn scenic-score [tree surroundings]
  (reduce * (map #(count-visible tree %) surroundings)))

(defn get-surroundings [trees x y]
  (vector
   (reverse (take x (nth trees y))) ;left
   (drop (+ x 1) (nth trees y)) ;right
   (reverse (take y (nth (apply map vector trees) x))) ;above
   (drop (+ y 1) (nth (apply map vector trees) x)) ; below
   ))

(defn score-trees [trees]
  (for [y (range (count trees)) x (range (count (first trees)))]
    (scenic-score (nth (nth trees y) x) (get-surroundings trees x y))))

(defn part-1 [input]
  (println
    (->>
     (str/split input #"\n")
     (map #(mapv parse-input %))
     (map mark-row)
     (map reverse)
     (map mark-row)
     (apply map vector)
     (map mark-row)
     (map reverse)
     (map mark-row)
     (flatten)
     (filter #(% :visible))
     (count))))

(defn part-2 [input]
  (println
   (->>
    (str/split input #"\n")
    (map #(mapv (fn [tree] (Character/digit tree 10)) %))
    (score-trees)
    (apply max)
    )))

(part-1 (slurp "inputs/day8"))
(part-2 (slurp "inputs/day8"))
