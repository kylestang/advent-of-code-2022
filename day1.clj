(ns day1)

(require '[clojure.string :as str])

(defn split-elves [elf-list] (str/split elf-list #"\n\n"))

(defn split-food [food-list] (str/split food-list #"\n"))

(defn sum-ints [foods] (reduce + (map read-string foods)))

(defn elf-values [elf-list]
  (map sum-ints (map split-food (split-elves elf-list))))

(defn part-1 [input]
  (println (apply max (elf-values input))))

(defn part-2 [input]
  (println (reduce + (take-last 3 (sort (elf-values input))))))

(defn start [] ((juxt part-1 part-2) (slurp "inputs/day1")))

(start)
