(ns tick-tack.core
  (:gen-class)
  (:require [clojure.core.async :as a :refer [<!!]]
            [clojure.string :as s]
            [mount.core :as mount]
            [tick-tack.pprint :as p]
            [tick-tack.render :as r]
            [tick-tack.board :as b]))

(defn next-player
  [player]
  (case player
    :x :o
    :o :x))

(defn game-loop [next-move]
  (let [board (b/make-board)]
    (r/render-board board)
    (loop [board board
           player :x]
      (if-let [winner (b/won? board)]
        (do
          (r/render-board board)
          (printf "Player %s won!!" (next-player player))
          (flush)
          (next-player player))
        (do
          (r/render-board board)
          (printf "%s's turn - Insert the coordinates of where you want to play:\n" (s/upper-case (name player)))
          (flush)
          (let [coords (<!! next-move)]
            (if (b/available? coords board)
              (recur
                (assoc-in board coords player)
                (next-player player))
              (do
                (println "That position is already taken, please pick a different one")
                (recur board player)))))))))

(defn -main []
  (mount/start)
  (r/make-cursor-listener)
  (let [next-move (r/make-move-listener)]
    (loop [scores {:x 0 :o 0}]
      (let [winner (game-loop next-move)
            new-scores (update scores winner inc)]
        (println "\n\n--- Current score ---:")
        (printf "X: %s, O: %s\n" (:x new-scores) (:o new-scores))
        (println "--- New Game ---\n\n")
        (recur new-scores)))))
