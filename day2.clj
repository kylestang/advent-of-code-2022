(ns day2)

(require '[clojure.string :as str])

(defn evaluate [[other _ me]]
  (cond
    (= me \X) (cond
                (= other \A) 4
                (= other \B) 1
                :else 7)
    (= me \Y) (cond
                (= other \A) 8
                (= other \B) 5
                :else 2)
    :else (cond
            (= other \A) 3
            (= other \B) 9
            :else 6)))

(defn beats [shape]
  (cond
    (= shape \A) \Y
    (= shape \B) \Z
    :else \X))

(defn ties [shape]
  (cond
    (= shape \A) \X
    (= shape \B) \Y
    :else \Z))

(defn loses [shape]
  (cond
    (= shape \A) \Z
    (= shape \B) \X
    :else \Y))

(defn strategy [[other _ goal]]
  (cond
    (= goal \X) (str other " " (loses other))
    (= goal \Y) (str other " " (ties other))
    :else (str other " " (beats other))))

(defn part-1 [input]
  (println (->>
            (str/split input #"\n")
            (map evaluate)
            (reduce +))))

(defn part-2 [input]
  (println (->>
            (str/split input #"\n")
            (map strategy)
            (map evaluate)
            (reduce +))))

(defn start [] ((juxt part-1 part-2) (slurp "inputs/day2")))

(start)
