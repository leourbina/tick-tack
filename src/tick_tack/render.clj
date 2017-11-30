(ns tick-tack.render
  (:require [clojure.core.async :as a :refer [<! >! chan go go-loop]]
            [lanterna.screen :as sc]
            [mount.core :as mount]
            [tick-tack.pprint :refer [format-row make-divider]]
            [tick-tack.screen :refer [screen]]
            [tick-tack.board :refer [BOARD_SIZE]]))

(def START [4 2])
(def ^:private EDGE_WIDTH [2 1])
(def ^:private ASPECT_RATIO [4 2])

(def key->vector
  {:up    [-1  0]
   :down  [1  0]
   :left  [0 -1]
   :right [0  1]})

(defn apply-key
  [coords key]
  (map + coords (key->vector key)))

(defn coords->cursor
  ([coords] (coords->cursor coords START))
  ([coords start]
   (map +
     (map * ASPECT_RATIO (reverse coords))
     EDGE_WIDTH
     start)))

(defn cursor->coords
  ([cursor] (cursor->coords cursor START))
  ([cursor start]
   (reverse (map / (map - cursor EDGE_WIDTH start) ASPECT_RATIO))))

(defn in-range?
  [size coord]
  (and
    (>= coord 0)
    (< coord size)))

(defn coords-in-range?
  [coords size]
  (every? (partial in-range? size) coords))

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

(def valid-keys #{:up :down :left :right :enter})

(defn valid-key?
  [key]
  (some? (valid-keys key)))

(defn valid-position?
  [size start key]
  (-> (sc/get-cursor screen)
    (cursor->coords start)
    (apply-key key)
    (coords-in-range? size)))

(defn filter-keys
  [size start]
  (comp
    (filter valid-key?)
    (filter (partial valid-position? size start))))

(defn make-key-listener
  "Returns a channel that emits every time a key is pressed"
  [screen size start]
  (let [key-ch (chan 1 (filter-keys size start))]
    (go
      (while true
        (let [key (sc/get-key-blocking screen)]
          (>! key-ch key))))
    key-ch))

(mount/defstate
  key-listener
  :start (make-key-listener screen BOARD_SIZE START)
  :stop (a/close! key-listener))

(mount/defstate
  key-publisher
  :start (a/pub key-listener (constantly :keys)))

(defn make-cursor-listener
  ([] (make-cursor-listener START))
  ([start]
   (sc/move-cursor screen (coords->cursor [0 0] start))
   (sc/redraw screen)
   (let [moves-ch (chan 1 (comp
                            (filter (fn [key]
                                      (not= :enter key)))
                            (map (fn [key] #(apply-key % key)))))]
     (a/sub key-publisher :keys moves-ch)
     (go-loop [move (<! moves-ch)]
       (when (some? move)
         (let [next-cursor (-> screen
                             sc/get-cursor
                             (cursor->coords start)
                             move
                             (coords->cursor start))]
           (sc/move-cursor screen next-cursor)
           (sc/redraw screen)
           (recur (<! moves-ch))))))))

(defn make-move-listener
  ([] (make-move-listener START))
  ([start]
   (let [submit-ch (chan 1 (filter (partial = :enter)))
         play-ch (chan)]
     (a/sub key-publisher :keys submit-ch)
     (go-loop [submit (<! submit-ch)]
       (when submit
         (let [coords (-> screen
                        sc/get-cursor
                        (cursor->coords start))]
           (>! play-ch coords))
         (recur (<! submit-ch))))
     play-ch)))
