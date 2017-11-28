(ns tick-tack.core
  (:gen-class)
  (:require [tick-tack.pprint :as p]
            [clojure.string :as s]))

(def BOARD_SIZE 3)

(defn next-player
  [player]
  (case player
    :x :o
    :o :x))

(defn in-range?
  [size coord]
  (< coord size))

(defn valid?
  [size coords]
  (and
    (= (count coords) 2)
    (every? (partial in-range? size) coords)))

(defn make-board
  "Generates new empty board"
  [size]
  (let [row (vec (repeat size nil))
        board (vec (repeat size row))]
    board))

(defn available?
  [coords board]
  (nil? (get-in board coords)))

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

(defn game-loop []
  (let [board (make-board BOARD_SIZE)]
    (loop [board board
           player :x]
      (if-let [winner (won? board)]
        (do
          (printf "Player %s won!!" (next-player player))
          (flush)
          (next-player player))
        (do
          (p/print-board board)
          (printf "%s's turn: Insert the coordinates of where you want to play:\n" (s/upper-case (name player)))
          (flush)
          (let [coords (vec (map read-string
                              (-> (s/trim (read-line)) (s/split #" "))))]
            (if (valid? (count board) coords)
              (if (available? coords board)
                (recur
                  (assoc-in board coords player)
                  (next-player player))
                (do
                  (println "That position is already taken, please pick a different one")
                  (recur board player)))
              (do
                (println "The position you entered is invalid, please enter a valid position")
                (recur board player)))))))))

(defn -main []
  (let [scores {:x 0 :o 0}]
    (while true
      (do
        (let [winner (game-loop)
              scores (update scores winner inc)]
          (println "\n\n--- Current score ---:")
          (printf "X: %s, O: %s\n" (:x scores) (:o scores))
          (println "--- New Game ---\n\n"))))))
