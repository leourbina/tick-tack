(ns tick-tack.render
  (:require [lanterna.screen :as sc]
            [tick-tack.board :refer [BOARD_SIZE]]
            [tick-tack.pprint :refer [format-row make-divider]]
            [tick-tack.screen :refer [screen]]
            [tick-tack.controls :refer [START]]))

(defn render-board
  ([board] (render-board START board))
  ([start board]
   (let [original-cursor (sc/get-cursor screen)]
     (sc/move-cursor screen start)
     (let [size (count board)
           divider (make-divider size)
           [x y :as cursor] (sc/get-cursor screen)]
       (sc/put-string screen x y divider)
       (loop [cursor (map + [0 1] start)
              [row & rows] board]
         (when (some? row)
           (let [[x y] cursor]
             (sc/put-string screen x y (format-row row))
             (sc/put-string screen x (+ y 1) divider)
             (recur (map + [0 2] cursor) rows)))))
     (sc/move-cursor screen original-cursor)
     (sc/redraw screen))))

(defn render-scores
  ([scores] (render-scores scores START))
  ([scores start]
   (let [[x y] (map + [0 (+ 2 (* 2 BOARD_SIZE))] start)]
     (sc/put-string screen x y (format "X: %s  Y: %s  Tie: %s"
                                 (scores :x)
                                 (scores :o)
                                 (scores :tie))))
   (sc/redraw screen)))
