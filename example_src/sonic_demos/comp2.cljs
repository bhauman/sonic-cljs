(ns sonic-demos.comp2
  (:require
   [devcards.core :as dc]
   [sonic-cljs.core :as sc :refer [n n* ch initial-player-state mloop music-root-card use-synth mloop* play-music! play-music!!
                                   controls analyzer]]
   [sonic-cljs.trig :refer [cosr quantize]]
   [sonic-cljs.pitch :as p]
   [sonic-cljs.webaudio :as wa]
   [sablono.core :as sab :include-macros true]) 
  (:require-macros
   [devcards.core :refer [defcard deftest]]))

(defonce music-state (atom (initial-player-state
                            {:synth @sc/default-synth
                             :speed 0.30
                             :start-time (sc/current-time* {:speed 0.30})})))

(defn beat [b]
  (iterate #(+ % b) 0))

(def sixteenth-beat (beat (/ 1 16)))

(def eigth-beat (beat (/ 1 8)))

(defn right-hand-n [scale root beat]
  (quantize
   (int
    (cosr beat (cosr beat 3 5 2)
          (+ (sc/note root) 12)
          (/ 3 7))) scale))

(defn right-hand-high [scale root beat]
  (if (< (rand) 0.6)
    (quantize
     (+ 7
        (int
         (cosr beat
               (cosr beat 3 5 2)
               (+ (sc/note root) 24)
               (/ 3 7))))
     scale)
    ::sc/rest))

(def scale (p/scale-field :G :major-pentatonic))

(defn music-note [scale root beat]
  (n
   (right-hand-n scale root beat)
   (/ 0.25 3)
   (cosr beat 0.4 0.6 (/ 3 7))
   0.3))

(defn music-note-high [scale root beat]
  (n
   (right-hand-high scale root beat)
   (/ 0.25 4)
   (cosr beat 0.15 0.6 (/ 3 7))
   0.05))

(def root-note
  (iterate
   (fn [x]
     (rand-nth
          (seq (disj #{:B3 :D3 :c3 #_"F#3"} x))))
   :B3))

(defn base-note [bnote root beat]
  [(n bnote 0.125 0.5 0.35)
   (n root 0.125 0.4 0.35)])

(defonce main-synth*
  #_(wa/poly-synth wa/fm-synth {:decay 0.4
                              :sustain 0.1
                              :sustain-level 0.5
                              :release 0.2
                              :amp 0.3
                              :modulation-index 25
                              :harmonicity 3} {})
  (wa/piano wa/ivy-audio-piano))

(defonce main-synth2* (wa/piano wa/ivy-audio-piano)
  #_(wa/poly-synth wa/fm-synth
                 {:sustain-level 0.5
                  :decay 0.4
                  :sustain 0.1
                  :release 0.1
                  :amp 0.16
                  :modulation-index 33
                  :harmonicity 3 }
                 {:voices 10}))

(defonce main-synth3*
  (wa/piano wa/ivy-audio-piano)
  #_(wa/poly-synth wa/fm-synth {:attack 0.0
                              :decay  0.05
                              :sustain 0.1
                              :release 0.5
                              :amp 0.16
                              :modulation-index 40
                              :harmonicity 6}
                 {:voices 6}))

#_(swap! music-state assoc
       :speed 0.30
       :start-time (sc/current-time* {:speed 0.30}))

#_(defcard (analyzer))

(defcard (controls main-synth*))

(defcard bass-linee
  (fn [da o]
    (play-music!
     @da
     (use-synth
      main-synth*
      [:+
       (mapcat
        #(base-note %1 %2 %3)
        (mloop* [:e3 :e3 :g3 :f#3 #_:g3])
        (mapcat #(take 8 (repeat %)) root-note)
        (beat 0.25))])))
  music-state)

(defcard (controls main-synth2*))

(defcard impro-threeee
  (fn [da o]
    (play-music!
     @da
     (use-synth
      main-synth2*
      [:+
      (map
       #(music-note scale %1 %2)
       (mapcat #(take 64 (repeat %)) root-note)
       (beat 0.5))])))
  music-state)

#_(defcard (controls main-synth3*))

(defcard impro-high
  (fn [da o]
    (play-music!
     @da
     (use-synth
      main-synth3*
      [:+
       (map
        #(music-note-high scale %1 %2)
        (mapcat
         #(take 64 (repeat %))
         root-note)
        (beat 0.25))])))
  music-state)

(defonce bass-synth
  (wa/fm-synth {:attack 0.015
                :decay 0.09
                :sustain 0.1 :release 0.2 :amp 0.5 :modulation-index 20 :harmonicity 3}))

(defcard (controls bass-synth))

(defcard low-base-line
  (fn [da o]
    (play-music!
     @da
     (use-synth
      bass-synth
      [:+
       (map
        #(n (- (p/note %1)
               12) %2 0.7 0.2)
        (mapcat #(take 6 (repeat %)) root-note)
        (mloop* [(+ 0.25 0.125) 0.25 (- 0.5 0.125)]))])))
  music-state)

(defonce hh-synth2 (wa/sampler {44 "audio/505/hh.mp3" } {:amp 0.2 :spike 0.05}))
(defonce hh-synth (wa/noise {:amp 0.033
                             :spike 0.01
                             :decay 0.02
                             :attack 0.0
                             :sustain 0.01
                             :sustain-level 0.5
                             :release 0.01 }))

(defcard kick
  (fn [da o]
    (play-music!
     @da
     (use-synth
      hh-synth2
      [:+
       (mapcat
        (fn [b]
          [(n 44 (/ 0.25 3) 1 0.01)
           (n 44 (- 0.33 (/ 0.25 3)) 0.2 0.1)])
        (beat 0.125))])))
  music-state)

(defcard hh
  (fn [da o]
    (play-music!
     @da
     (use-synth
      hh-synth
      [:+
       (map
        (fn [b]
          (n
           44
           (/ 0.25 3)
           (cosr b 0.5 0.5 (rand-nth [(/ 3 7) (/ 2 5)]))))
        (beat 0.125))])))
  music-state)
