(ns sonic-demos.core
  (:require
   [devcards.core :include-macros true]
   [sonic-cljs.core]
   [sonic-demos.apres-midi]
   [sonic-demos.sorensen]
   [sonic-demos.comp1]
   [sonic-demos.comp2]   
   ))

(defn ^:export main []
  (devcards.core/start-devcard-ui!))
