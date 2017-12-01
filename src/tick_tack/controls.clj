(ns tick-tack.controls
  (:require [clojure.core.async :as a :refer [<! >! chan go go-loop]]
            [lanterna.screen :as sc]
            [mount.core :as mount]
            [tick-tack.board :refer [BOARD_SIZE]]
            [tick-tack.screen :refer [screen]]))

(def START [4 2])
(def ^:private EDGE_WIDTH [2 1])
(def ^:private ASPECT_RATIO [4 2])

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

(def key->vector
  {:up    [-1  0]
   :down  [1  0]
   :left  [0 -1]
   :right [0  1]})

(defn apply-key
  [coords key]
  (map + coords (key->vector key)))

(defn in-range?
  [size coord]
  (and
    (>= coord 0)
    (< coord size)))

(defn coords-in-range?
  [coords size]
  (every? (partial in-range? size) coords))

(def valid-keys #{:up :down :left :right :enter \q \space})

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
                            (filter #{:up :down :left :right})
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
   (let [submit-ch (chan 1 (filter #{:enter \space}))
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

(defn stop-app []
  (mount/stop)
  (shutdown-agents))

(defn make-quit-listener []
  (let [quit-ch (chan 1 (filter #{\q}))]
    (a/sub key-publisher :keys quit-ch)
    (go-loop [quit (<! quit-ch)]
      (when quit
        (stop-app)))))
