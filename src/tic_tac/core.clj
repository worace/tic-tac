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

(defn get-vector-values [board win-vec] (map board win-vec))

(defn winning-player? [board player]
  (let [winning-sequence (take (size board)
                               (repeat player))]
    (->> (win-vectors board)
         (map (partial get-vector-values board))
         (filter (partial = winning-sequence))
         (first))))

(defn winner [board]
  (first (filter (partial winning-player? board)
                 ["X" "O"])))
