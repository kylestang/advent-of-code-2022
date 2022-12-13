(ns day3)

(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn priority [[item]]
  (if (Character/isUpperCase item)
    (- (int item) 38)
    (- (int item) 96)))

(defn split-string [input]
  (split-at (quot (count input) 2) input))

(defn duplicates [items]
  (->>
   (map set items)
   (apply set/intersection)
   (seq)))

(defn part-1 [input] 
  (println (->>
            (str/split input #"\n")
            (map split-string)
            (map duplicates)
            (map priority)
            (reduce +))))

(defn part-2 [input]
  (println (->> 
            (str/split input #"\n")
            (partition 3)
            (map duplicates)
            (map priority)
            (reduce +))))

(defn start [] ((juxt part-1 part-2) (slurp "inputs/day3")))

(start)
