(ns tick-tack.core-test
  (:require [tick-tack.core :refer :all]
            [midje.sweet :refer :all]))

(facts "About boards"
  (fact "Can create new board"
    (new-board 1) => [[nil]]
    (new-board 2) => [[nil nil] [nil nil]]
    (new-board 3) => [[nil nil nil] [nil nil nil] [nil nil nil]])

  (fact "Can check whether a position is available"
    (let [board (-> (new-board 3)
                  (assoc-in [0 1] :x))]
      (available? [0 0] board) => true
      (available? [0 1] board) => false))

  (fact "Can check whether a board has been won"
    (won? [[:x :x :x]
           [nil nil nil]
           [nil nil nil]]) => :x

    (won? [[nil nil nil]
           [:x :x :x]
           [nil nil nil]]) => :x

    (won? [[nil nil nil]
           [nil nil nil]
           [:x :x :x]]) => :x

    (won? [[:x nil nil]
           [:x nil nil]
           [:x nil nil]]) => :x

    (won? [[nil :x nil]
           [nil :x nil]
           [nil :x nil]]) => :x

    (won? [[nil nil :x]
           [nil nil :x]
           [nil nil :x]]) => :x

    (won? [[:x nil nil]
           [nil :x nil]
           [nil nil :x]]) => :x

    (won? [[nil nil :x]
           [nil :x nil]
           [:x nil nil]]) => :x))
