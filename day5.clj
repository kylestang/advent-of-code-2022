(ns day5)

(require '[clojure.string :as str])

(defn box-list [box-row]
  (map #(nth % 1) (partition 4 4 " " box-row)))

(defn remove-spaces [char-vec]
  (filter #(not= % \ ) char-vec))

(defn fix-indices [[amount source destination]]
  (list amount (dec source) (dec destination)))

(defn parse-input [[boxes moves]]
  (list
   (->> 
    (str/split boxes #"\n")
    (pop)
    (map box-list) 
    (apply map vector)
    (map reverse)
    (mapv remove-spaces))
   (->>
    (str/split moves #"\n") 
    (map #(str/split % #" "))
    (map rest)
    (map #(take-nth 2 %))
    (map #(map read-string %))
    (map fix-indices))
   ))

(defn move-single-boxes [boxes move]
  (let [[amount source destination] move] 
    (update 
     (update boxes destination concat 
             (reverse (take-last amount (nth boxes source))))
     source #(drop-last amount %))))

(defn move-many-boxes [boxes move]
  (let [[amount source destination] move]
    (update
     (update boxes destination concat 
             (take-last amount (nth boxes source)))
     source #(drop-last amount %))))

(defn part-1 [input] 
  (println
   (let [[boxes moves] 
         (parse-input (str/split input #"\n\n"))] 
     (->>
      (reduce move-single-boxes boxes moves)
      (map reverse)
      (apply map vector)
      (first)
      (apply str)))))

(defn part-2 [input]
  (println
   (let [[boxes moves]
         (parse-input (str/split input #"\n\n"))]
     (->>
      (reduce move-many-boxes boxes moves)
      (map reverse)
      (apply map vector)
      (first)
      (apply str)))))

(defn start []
  ((juxt part-1 part-2) (slurp "inputs/day5")))

(start)
