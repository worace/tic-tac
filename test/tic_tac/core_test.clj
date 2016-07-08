(ns tic-tac.core-test
  (:require [clojure.test :refer :all]
            [tic-tac.core :refer :all]
            [tic-tac.display :refer [board-status-string
                                     prompt-string]]))

(deftest making-a-grid
  (is (= 9 (count (board 3))))
  (is (some #{"A0"} (keys (board 3))))
  (is (some #{"C2"} (keys (board 3))))
  (is (nil? (get (board 3) "C2"))))


(deftest figuring-out-win-vectors
  (let [b (board 3)]
    (is (= 3 (count (rows b))))
    (is (= ["A0" "A1" "A2"] (first (rows b))))
    (is (= 3 (count (cols b))))
    (is (= ["A2" "B2" "C2"] (last (cols b))))
    (is (= 2 (count (diags b))))
    (is (= [["A0" "B1" "C2"]
            ["A2" "B1" "C0"]] (diags b)))
    (is (= [;; Rows
            ["A0" "A1" "A2"]
            ["B0" "B1" "B2"]
            ["C0" "C1" "C2"]
            ;; Cols
            ["A0" "B0" "C0"]
            ["A1" "B1" "C1"]
            ["A2" "B2" "C2"]
            ;; Diags
            ["A0" "B1" "C2"]
            ["A2" "B1" "C0"]]
           (win-vectors b)))))

(deftest identifying-winning-boards
  (is (nil? (winner (board 3))))
  (is (= "X" (-> (board 3)
                 (assoc "A0" "X" "A1" "X" "A2" "X")
                 (winner))))
  (is (= "O" (-> (board 3)
                 (assoc "A0" "O" "B1" "O" "C2" "O")
                 (winner)))))

;; A0|A1|A2
;; --------
;; B0|B1|B2
;; --------
;; C0|C1|C2

;; {"A0" nil, "A1" nil, "A2" nil, "B0" nil, "B1" nil, "B2" nil, "C0" nil, "C1" nil, "C2" nil}
;; {"A0" "X", "A1" "X", "A2" "X", "B0" nil, "B1" nil, "B2" nil, "C0" nil, "C1" nil, "C2" nil}

(deftest identify-drawn-boards
  (is (not (drawn? (board 3))))
  ;; X| |O
  ;; -----
  ;; O|O|X
  ;; -----
  ;; X|X|O
  (is (-> (board 3)
          (assoc "A0" "X" "B2" "X" "C0" "X" "C1" "X")
          (assoc "A2" "O" "B0" "O" "B1" "O" "C2" "O")
          (drawn?))))

(deftest printing-a-board
  (is (= " | | \n-----\n | | \n-----\n | | "
         (board-status-string (board 3))))
  (is (= " | | \n-----\n |X| \n-----\nO| | "
         (board-status-string (assoc (board 3) "B1" "X" "C0" "O")))))

(deftest prompting-for-a-move
  (is (= "A0|A1|A2\n--------\nB0|B1|B2\n--------\nC0|C1|C2"
         (prompt-string (board 3))))
  (is (= "A0|**|A2\n--------\nB0|B1|B2\n--------\n**|C1|C2"
         (prompt-string (assoc (board 3) "A1" "X" "C0" "O")))))

(def drawn-board (-> (board 3)
                     (assoc "A0" "X" "B2" "X" "C0" "X" "C1" "X")
                     (assoc "A2" "O" "B0" "O" "B1" "O" "C2" "O")))

(deftest scoring-a-board
  (let [b (board 3)]
    (is (= 0 (score b "X")))
    (is (= 100 (score (assoc b "A0" "X" "A1" "X" "A2" "X")
                      "X")))
    (is (= -100 (score (assoc b "A0" "O" "A1" "O" "A2" "O")
                      "X")))
    (is (= 0 (score drawn-board "X")))
    ))

(deftest computer-makes-winning-move
  (let [b (-> (board 3)
              (assoc "A0" "X"
                     "A1" "O"
                     "B1" "X"
                     "A2" "O"))]
    (is (= "C2" (best-move b "X")))))

(deftest computer-blocks-winning-move
  (let [b (-> (board 3)
              (assoc "A0" "O"
                     "A1" "X"
                     "B1" "O"))]
    (is (= "C2" (best-move b "X"))))
  (let [b (-> (board 3)
              (assoc "A0" "O"
                     "A1" "O"
                     "C2" "X"))]
    (is (= "A2" (best-move b "X"))))
  (let [b (-> (board 3)
              (assoc "C0" "O"
                     "B1" "O"
                     "C2" "X"))]
    (is (= "A2" (best-move b "X")))))

;; drawn
;; {"A0" "X", "A1" nil, "A2" "O",
;;  "B0" "O", "B1" "O", "B2" "X",
;;  "C0" "X", "C1" "X", "C2" "O"}

;; TicTacToe Scoring
;; Win: 100 points
;; Loss: -100 points
;; Draw: 0 points
;; 2 in a row: 20 points (??)
;; 2 in a row opponent: - 20 points (??)

;; Min-Maxing
;; 1 - need to calculate a "score" for given game states
;;   - score is from perspective of a given player (X/O)
;; 2 - at each turn, need to calculate score for
;;     each possible move
;;   - take the move that returns the highest score
;;
;; Depth:
;; - specify how many iterations to look in each dir?
