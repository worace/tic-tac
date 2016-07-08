(ns tic-tac.display)

(defn size [board] (int (Math/sqrt (count board))))

(defn row-fence [length] (apply str (take length (repeat "-"))))

(defn board-string [square-display-fn board]
  (->> (sort board)
       (map square-display-fn)
       (partition (size board))
       (map (partial clojure.string/join "|"))
       (mapcat (fn [row-str] [row-str
                              (row-fence (count row-str))]))
       (drop-last)
       (clojure.string/join "\n")))

(def board-status-string (partial board-string
                                  (fn [[square value]]
                                    (or value " "))))

(def prompt-string (partial board-string
                            (fn [[square value]]
                              (if (nil? value)
                                square
                                "**"))))
