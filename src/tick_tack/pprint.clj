(ns tick-tack.pprint
  (:require [clojure.string :as s]))

(defn make-divider
  [size]
  (s/join (concat (take size (repeat "+---")) "+")))

(defn format-row
  [row]
  (s/join (concat (map (fn [x] (format "| %s " (if x (name x) " "))) row) "|")))

(defn print-board
  [board]
  (let [size (count board)
        divider (make-divider size)]
    (println divider)
    (doseq [row board]
      (do
        (println (format-row row))
        (println divider)))))
