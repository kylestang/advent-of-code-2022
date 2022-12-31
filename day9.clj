(ns day9)

(require '[clojure.string :as str])

(defn parse-input [row]
  (let [[direction num] (str/split row #" ")]
    (repeat
     (read-string num)
     (cond
       (= direction "L") {:x -1 :y 0}
       (= direction "R") {:x 1 :y 0}
       (= direction "U") {:x 0 :y 1}
       :else {:x 0 :y -1}))))

(defn should-move [high low normal1 normal2]
  (or
   (> (- high low) 1)
   (and (> (- high low) 0) (> (abs (- normal1 normal2)) 1))))

(defn update-head [head move]
  {:x (+ (head :x) (move :x)) :y (+ (head :y) (move :y))})

(defn update-tail [state knot]
  (update state
          :knots
          conj
          (let
           [head-x ((peek (state :knots)) :x)
            head-y ((peek (state :knots)) :y)
            tail-x (knot :x)
            tail-y (knot :y)]
            (cond-> knot
              (should-move head-x tail-x head-y tail-y) ; head is to the right of tail
              (update :x inc)
              (should-move tail-x head-x head-y tail-y) ; head is to the left of tail
              (update :x dec)
              (should-move head-y tail-y head-x tail-x) ; head is above tail
              (update :y inc)
              (should-move tail-y head-y head-x tail-x) ; head is below tail
              (update :y dec)))))


(defn simulate-rope [state move]
  (let [new-state (reduce
                   update-tail
                   {:visited (state :visited)
                    :knots [(update-head (first (state :knots)) move)]}
                   (rest (state :knots)))]
    (update new-state :visited conj (last (new-state :knots)))))

(defn part-1 [input]
  (println
    (count
     ((->>
       (str/split input #"\n")
       (map parse-input)
       (flatten)
       (reduce simulate-rope {:visited #{{:x 0 :y 0}} :knots (repeat 2 {:x 0 :y 0})}))
      :visited))))

(defn part-2 [input]
  (println
   (count
    ((->>
      (str/split input #"\n")
      (map parse-input)
      (flatten)
      (reduce simulate-rope {:visited #{{:x 0 :y 0}} :knots (repeat 10 {:x 0 :y 0})}))
     :visited))))

(part-1 (slurp "inputs/day9"))
(part-2 (slurp "inputs/day9"))
