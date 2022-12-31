(ns day10)

(require '[clojure.string :as str])

(defn update-screen [state]
  (if (<= (abs (- (state :x) (mod (dec (state :cycle)) 40))) 1)
    (update state :screen conj \#)
    (update state :screen conj \.)))

(defn update-signal [state]
  (if
   (or
    (= (state :cycle) 20)
    (= (state :cycle) 60)
    (= (state :cycle) 100)
    (= (state :cycle) 140)
    (= (state :cycle) 180)
    (= (state :cycle) 220))
    (update state :signal + (* (state :x) (state :cycle)))
    state))

(defn do-cycle [state]
  (-> state
      (update-screen)
      (update-signal)
      (update :cycle inc)))

(defn addx [state value]
  (-> state
      (do-cycle)
      (do-cycle)
      (update :x + value)))

(defn noop [state]
  (do-cycle state))

(defn operation [state [instruction value]]
  (if (= instruction "addx")
    (addx state (read-string value))
    (noop state)))

(defn run-device [input]
  (->>
   (str/split input #"\n")
   (map #(str/split % #" "))
   (reduce operation {:signal 0 :cycle 1 :x 1 :screen []})))

(defn part-1 [input]
  (println ((run-device input) :signal)))

(defn part-2 [input]
  (doseq [line (partition 40 ((run-device input) :screen))]
    (println (apply str line))))

(part-1 (slurp "inputs/day10"))
(part-2 (slurp "inputs/day10"))
