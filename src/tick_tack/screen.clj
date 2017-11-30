(ns tick-tack.screen
  (:require [mount.core :as mount]
            [lanterna.screen :as sc]))

(mount/defstate
  screen
  :start (let [screen (sc/get-screen)]
           (sc/start screen)
           screen)
  :stop (sc/stop screen))
