(ns day7)

(require '[clojure.string :as str])

(defn update-size [dir-list add-size]
  (mapv #(update % :size + add-size) dir-list))

(defn dir-size [fs entry]
  (let [words (str/split entry #" ")]
    (cond
      (re-find #"^\$ cd \.\." entry)
      (update (update fs :complete-dirs conj (last (fs :current-dirs))) :current-dirs pop)
      (re-find #"^\$ cd" entry)
      (update fs :current-dirs conj {:name (nth words 2) :size 0})
      (re-find #"^\d" entry)
      (update fs :current-dirs update-size (read-string (first words)))
      :else fs)))

(defn part-1 [input]
  (println
   (->>
    (str/split input #"\n")
    (reduce dir-size {:current-dirs [] :complete-dirs []})
    (vals)
    (apply concat)
    (map #(% :size))
    (filter #(<= % 100000))
    (reduce +))))

(defn part-2 [input]
  (println
   (let [dir-sizes 
         (->> 
          (str/split input #"\n") 
          (reduce dir-size {:current-dirs [] :complete-dirs []}) 
          (vals) 
          (apply concat) 
          (map #(% :size)))]
     (apply min (filter #(>= % (- (first dir-sizes) 40000000)) dir-sizes)))))

(defn start []
  ((juxt part-1 part-2) (slurp "inputs/day7")))

(start)
