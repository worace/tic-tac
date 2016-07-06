(ns tic-tac.core)

(defn alphabet [] (map (comp str char) (iterate inc (int \A))))

(defn board [size]
  (zipmap (mapcat (fn [row]
                    (for [col (range size)]
                      (str row col)))
                  (take size (alphabet)))
          (repeat nil)))
