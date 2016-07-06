(ns tic-tac.core-test
  (:require [clojure.test :refer :all]
            [tic-tac.core :refer :all]))

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
            ["A2" "B1" "C0"]
            ]
           (win-vectors b)))))

(deftest identifying-winning-boards
  (is (nil? (winner (board 3))))
  (is (= "X" (-> (board 3)
                 (assoc "A0" "X" "A1" "X" "A2" "X")
                 (winner))))
  (is (= "O" (-> (board 3)
                 (assoc "A0" "O" "B1" "O" "C2" "O")
                 (winner)))))
