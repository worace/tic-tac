(ns tic-tac.core-test
  (:require [clojure.test :refer :all]
            [tic-tac.core :refer :all]))

(deftest making-a-grid
  (is (= 9 (count (board 3))))
  (is (some #{"A0"} (keys (board 3))))
  (is (some #{"C2"} (keys (board 3))))
  (is (nil? (get (board 3) "C2"))))
