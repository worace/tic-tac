(ns tic-tac.core)

(defn alphabet [] (map (comp str char) (iterate inc (int \A))))

(defn size [board] (int (Math/sqrt (count board))))

(defn transpose [m]
  (apply mapv vector m))

(defn grid [size]
  (for [row (take size (alphabet))]
    (for [col (range size)]
      (str row col))))

(defn board [size]
  (zipmap (flatten (grid size))
          (repeat nil)))

(defn rows [board] (grid (size board)))
(defn cols [board] (transpose (grid (size board))))

(defn forward-and-backward [seq]
  (concat seq (reverse seq)))

(defn diags [board]
  (->> [(cycle (take (size board) (alphabet)))
        (forward-and-backward (range (size board)))]
       (apply map str)
       (partition (size board))))

(defn win-vectors [board]
  (reduce concat ((juxt rows cols diags) board)))

(defn position-vals [board positions] (map board positions))

(def first-value (comp first filter))

(defn winning-player? [board player]
  (let [winning-sequence (take (size board)
                               (repeat player))]
    (->> (win-vectors board)
         (map (partial position-vals board))
         (first-value (partial = winning-sequence)))))

(defn winner [board]
  (first-value (partial winning-player? board)
               ["X" "O"]))

(defn compact [coll] (filter (comp not nil?) coll))

(defn drawn? [board]
  (->> (win-vectors board)
       (map (partial position-vals board))
       (map compact)
       (map set)
       (every? (partial = #{"X" "O"}))))

(def next-turn {"O" "X" "X" "O"})

(defn prompt-move [board]
  (loop [selection (read-line)]
    (cond
      (not (contains? board selection)) (do (println "Sorry," selection "is not a valid move.")
                                            (recur (read-line)))
      (not (nil? (board selection))) (do (println "Sorry," selection "is not an open square.")
                                         (recur (read-line)))
      :else selection)))

(defn empty-squares [board]
  (->> board
       (filter (fn [[k v]] (nil? v)))
       (map first)))

(defn computer-move [board]
  (rand-nth (empty-squares board)))

(defn get-move [board player]
  (case player
    "X" (computer-move board)
    "O" (prompt-move board)))

(defn play []
  ;; make board
  ;; prompt player for move ("X" by default?)
  ;; print view of available cells...
  (loop [b (board 3)
         current-player "O"]
    (cond
      (winner b) (println (winner b) "wins!")
      (drawn? b) (println "Sorry, game is a draw...")
      :else (recur (assoc b (get-move b current-player) current-player)
                   (next-turn current-player)))))
