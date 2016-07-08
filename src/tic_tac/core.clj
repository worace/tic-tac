(ns tic-tac.core
  (:require [clojure.string :refer [join]]
            [tic-tac.display :refer [prompt-string
                                     board-status-string]]))

(def opponent {"O" "X" "X" "O"})

(defn alphabet [] (map (comp str char) (iterate inc (int \A))))

(defn size [board] (int (Math/sqrt (count board))))

(defn transpose [m] (apply mapv vector m))

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

(defn score [board player]
  (cond
    (= player (winner board)) 100
    (= (opponent player) (winner board)) -100
    (drawn? board) 0
    :else 0))

(defn empty-squares [board]
  (->> board
       (filter (fn [[k v]] (nil? v)))
       (map first)))

(defn outcomes-map [move outcomes player]
  {:move move
   :outcomes outcomes
   :min-score (->> outcomes
                   (map #(score % player))
                   (apply min))})

(defn final-state? [board] (or (winner board) (drawn? board)))

(defn game-tree [board current-player]
  (let [move-outcomes (map (fn [move]
                             (let [new-board (assoc board move current-player)
                                   outcomes (if (final-state? new-board)
                                              [new-board]
                                              (game-tree new-board (opponent current-player)))]
                               (outcomes-map move outcomes current-player)
                               ))
                           (empty-squares board))]
    (->> move-outcomes
         (apply max-key :min-score)
         (:outcomes))))

(defn best-move [board player]
  (apply max-key (fn [square]
                   (score (apply min-key
                                 (fn [b] (score b player))
                                 (game-tree (assoc board square player)
                                            (opponent player)))
                          player))
         (empty-squares board)))

(defn prompt-move [board]
  (println "Choose a move from the open spaces:")
  (println (prompt-string board))
  (loop [selection (read-line)]
    (cond
      (not (contains? board selection)) (do (println "Sorry," selection "is not a valid move.")
                                            (recur (read-line)))
      (not (nil? (board selection))) (do (println "Sorry," selection "is not an open square.")
                                         (recur (read-line)))
      :else selection)))

(defn get-move [board player]
  (case player
    "X" (best-move board player)
    "O" (prompt-move board)))

(defn play []
  ;; make board
  ;; prompt player for move ("X" by default?)
  ;; print view of available cells...
  (loop [b (board 3)
         current-player "O"]
    (println "Current Board:")
    (println (board-status-string b))
    (cond
      (winner b) (println (winner b) "wins!")
      (drawn? b) (println "Sorry, game is a draw...")
      :else (recur (assoc b (get-move b current-player) current-player)
                   (opponent current-player)))))

(defn -main [& args] (play))
