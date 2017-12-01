(ns tick-tack.core
  (:gen-class)
  (:require [clojure.core.async :as a :refer [<!!]]
            [mount.core :as mount]
            [tick-tack.board :as b]
            [tick-tack.render :as r]
            [tick-tack.controls :as c]))

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
      (if-let [winner (b/game-over? board)]
        (do
          (r/render-board board)
          winner)
        (do
          (r/render-board board)
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
  (c/make-cursor-listener)
  (c/make-quit-listener)
  (.addShutdownHook (Runtime/getRuntime) (Thread. c/stop-app))
  (let [next-move (c/make-move-listener)
        scores {:x 0 :o 0 :tie 0}]
    (r/render-scores scores)
    (loop [scores scores]
      (let [winner (game-loop next-move)
            new-scores (update scores winner inc)]
        (r/render-scores new-scores)
        (recur new-scores)))))
