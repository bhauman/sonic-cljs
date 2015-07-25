(ns sonic-demos.core
  (:require
   [devcards.core :include-macros true]
   [sonic-cljs.core]
   [sonic-demos.apres-midi]
   [sonic-demos.sorensen]

   #_[sonic-demos.webaudio]
   #_[sonic-demos.gibberish]
   ))


(defn ^:export main []
  (devcards.core/start-devcard-ui!))
