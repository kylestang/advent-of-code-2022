(ns day11 
  (:require [clojure.string :as str]))

(defn parse-input [[id items operation test true-monkey false-monkey]]
  {:id (read-string (subs id 7 (dec (count id))))
   :items (map read-string (str/split (subs items 18) #", "))
   :operator (nth operation 23)
   :operand (if (= (subs operation 25) "old") nil (read-string (subs operation 25)))
   :test (read-string (subs test 21))
   :true-monkey (read-string (subs true-monkey 29))
   :false-monkey (read-string (subs false-monkey 30))
   :inspections 0})

(defn test-condition [monkey item]
  (if (= (mod item (monkey :test)) 0)
    (monkey :true-monkey)
    (monkey :false-monkey)))

(defn update-worry [monkey item]
  (let [operand (if (nil? (monkey :operand)) item (monkey :operand))]
    (quot
     (if (= (monkey :operator) \+)
       (+ item operand)
       (* item operand))
     1))) ; Change to 3 for part 1

(defn inspect [monkey-state item]
  (let [monkey (monkey-state :monkey) item (update-worry monkey item)]
   (-> monkey-state
    (update-in
     [:list (test-condition monkey item) :items]
     conj
     (mod item 9699690)) ; Least common denominator of the monkey's tests
    (update-in [:list (monkey :id) :inspections] inc))))

(defn f [monkey-list monkey]
  (->
   ((reduce inspect {:list monkey-list :monkey monkey} (get-in monkey-list [(monkey :id) :items])) :list)
   (update-in [(monkey :id) :items] empty)))

(defn round [monkey-list]
  (reduce f monkey-list monkey-list))

(defn part-1 [input]
  (println
   (reduce *
           (->>
            (str/split input #"\n\n")
            (map #(str/split % #"\n"))
            (mapv parse-input)
            (#(nth (iterate round %) 20))
            (map #(% :inspections))
            (sort)
            (take-last 2)))))

(defn part-2 [input]
  (println
   (reduce *
           (->>
            (str/split input #"\n\n")
            (map #(str/split % #"\n"))
            (mapv parse-input)
            (#(nth (iterate round %) 10000))
            (map #(% :inspections))
            (sort)
            (take-last 2)))))

(part-1 (slurp "inputs/day11"))
(part-2 (slurp "inputs/day11"))
