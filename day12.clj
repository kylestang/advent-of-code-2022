(ns day12) 

(defn normalize-heights [input]
  (mapv
   (fn [tile]
     (cond
       (= tile (int \S)) (int \a)
       (= tile (int \E)) (int \z)
       :else tile))
   input))

(defn get-up [node width]
  (-> node
      (update :pos - width)
      (update :depth inc)))

(defn get-down [node width]
  (-> node
      (update :pos + width)
      (update :depth inc)))

(defn get-left [node] 
  (-> node
      (update :pos dec)
      (update :depth inc)))

(defn get-right [node]
  (-> node
      (update :pos inc)
      (update :depth inc)))

(defn valid-move [current-pos width next heightmap visited]
  (and 
   (>= next 0) ; Not above top
   (< next (count heightmap)) ; Not below bottom
   (not (contains? visited next)) ; Not already visited
   (not (and (= (mod current-pos width) 0) (= next (dec current-pos)))) ; Not left border
   (not (and (= (mod current-pos width) (dec width)) (= next (inc current-pos)))); Not right border
   (<= (- (heightmap next) (heightmap current-pos)) 1))) ; Max 1 higher

(defn update-queue [state next]
  (if (valid-move
       ((first (state :queue)) :pos)
       (state :width)
       (next :pos)
       (state :heightmap)
       (state :visited))
    (-> state
        (update :queue concat [next])
        (update :visited conj {(next :pos) (next :depth)}))
    state))

(defn search [state]
  (let [width (state :width) node (first (state :queue))]
    (-> state
        (update-queue (get-up node width))
        (update-queue (get-down node width))
        (update-queue (get-left node))
        (update-queue (get-right node))
        (update :queue rest))))

(defn part-1-iter [width heightmap pos end]
  (some
   #((% :visited) end)
   (iterate
    search
    {:queue (conj '() {:pos pos :depth 0}) :width width :heightmap heightmap :visited {}})))

(defn part-2-iter [width heightmap end]
  (some
   #((% :visited) end)
   (iterate
    search
    {:queue 
     (map 
      (fn [pos] {:pos pos :depth 0})
      (map (fn [[index _]] index)
           (filter
            (fn [[_ value]] (= value (int \a)))
            (map-indexed
             (fn [index value] [index value]) heightmap))))
     :width width 
     :heightmap heightmap 
     :visited {}})))

(defn find-char [heightmap character]
  (count (take-while #(not= % character) heightmap)))

(defn part-1 [input]
  (println
   (let [heightmap (filterv #(not= % 10) (map int input))]
     (part-1-iter
      (find-char (map int input) 10)
      (normalize-heights heightmap)
      (find-char heightmap (int \S))
      (find-char heightmap (int \E))))))

(defn part-2 [input]
  (println
   (let [heightmap (filterv #(not= % 10) (map int input))]
     (part-2-iter
      (find-char (map int input) 10)
      (normalize-heights heightmap)
      (find-char heightmap (int \E))))))

(part-1 (slurp "inputs/day12"))
(part-2 (slurp "inputs/day12"))
