(ns sonic-demos.sorensen
  (:require
   [devcards.core :as dc]
   [sonic-cljs.core :as sc :refer [n ch initial-player-state mloop music-root-card use-synth main-synth]]
   [sablono.core :as sab :include-macros true])
  (:require-macros
   [devcards.core :refer [defcard deftest]]))

(defcard hello)
