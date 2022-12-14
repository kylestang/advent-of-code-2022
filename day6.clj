(ns day6)

(defn part-1 [input]
  (println (+ (count (take-while #(not= (count (distinct %)) 4) (partition 4 1 input))) 4)))

(defn part-2 [input]
  (println (+ (count (take-while #(not= (count (distinct %)) 14) (partition 14 1 input))) 14)))

(defn start [] 
  ((juxt part-1 part-2) (slurp "inputs/day6")))

(start)
