(ns tick-tack.board)

(def BOARD_SIZE 3)

(defn available?
  [coords board]
  (nil? (get-in board coords)))

(defn make-board
  "Generates new empty board"
  ([] (make-board BOARD_SIZE))
  ([size]
   (let [row (vec (repeat size nil))
         board (vec (repeat size row))]
     board)))

(defn place-piece
  "Places piece in board, only if it is possible"
  [piece coords board]
  (if (available? coords board)
    [true (assoc-in board coords piece)]
    [false board]))

(defn make-checker
  "Returns checker for given start and offset

  A checker is a function that takes a board and returns whether all pieces in
  the given row/column/diagonal are the same and not nil"
  [start offset]
  (fn [board]
    (let [size (count board)
          positions (map (fn [multiple]
                           (->> offset
                             (map * (repeat multiple))
                             (map + start)
                             (map #(mod % size))))
                      (take size (next (range))))]
      (and
        (apply = (map (partial get-in board) positions))
        (get-in board (first positions))))))

(defn won?
  "Returns the player that won, or nil if none yet"
  ([board]
   (let [size (count board)
         row-checkers    (for [x (range size)] (make-checker [x 0] [0 1]))
         column-checkers (for [y (range size)] (make-checker [0 y] [1 0]))
         diag1-checker   (make-checker  [0 0] [1 1])
         diag2-checker   (make-checker  [0 (- size 1)] [1 -1])]
     (won? board (concat row-checkers column-checkers [diag1-checker diag2-checker]))))
  ([board checks]
   (loop [[check & rest] checks]
     (if (nil? check)
       false
       (if-let [winner (check board)]
         winner
         (recur rest))))))

(defn tie?
  "Returns whether the board is tied"
  [board]
  (let [full? (fn [row]
                (every? some? row))]
    (every? full? board)))

(defn game-over?
  [board]
  (if-let [winner (won? board)]
    winner
    (when (tie? board)
      :tie)))
