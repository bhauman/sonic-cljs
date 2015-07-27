(ns sonic-demos.sorensen
  (:require
   [devcards.core :as dc]
   [sonic-cljs.core :as sc :refer [n n* ch initial-player-state mloop music-root-card use-synth mloop* play-music! play-music!!]]
   [sonic-cljs.trig :refer [cosr quantize]]
   [sonic-cljs.pitch :as p]
   [sonic-cljs.webaudio :as wa]
   [sablono.core :as sab :include-macros true]) 
  (:require-macros
   [devcards.core :refer [defcard deftest]]))

(defn quantizer
  [midi field]
  (let [nt midi]
    (if (some #{nt} field)
      nt
      (loop [x 1]
        (let [up (int (+ nt x))
              down (int (- nt x))]
          (cond
           (some #{up} field) up
           (some #{down} field) down
           :else (recur (inc x))))))))

(defonce music-state (atom (initial-player-state
                            {:synth @sc/default-synth
                             :speed 0.48
                             :start-time (sc/current-time* {:speed 0.48})})))

(defn beat [b]
  (iterate #(+ % b) 0))

(def sixteenth-beat (beat (/ 1 16)))

(def eigth-beat (beat (/ 1 8)))

(defn right-hand-n [scale root beat]
  (quantizer
   (int
    (cosr beat (cosr beat 3 5 2)
          (+ (sc/note root) 24)
          (/ 3 7))) scale))

(defn right-hand-high [scale root beat]
  (if (< (rand) 0.6)
    (quantizer
     (+ 7 (int
           (cosr beat
                 (cosr beat 3 5 2)
                 (+ (sc/note root) 24)
                 (/ 3 7))))
     scale)
    ::sc/rest))

(def scale (p/scale-field :E :aeolian))

(defn music-note [scale root beat]
  (n
   (right-hand-n scale root beat)
   (/ 0.25 4)
   (cosr beat 0.2 0.2 (/ 3 7))
   0.3))

(defn music-note-high [scale root beat]
  (n
   (right-hand-high scale root beat)
   (/ 0.25 4)
   (cosr beat 0.2 0.2 (/ 3 7))
   0.02))

(def root-note
  (iterate
    (fn [x]
      (- (rand-nth
          (seq (disj #{52 50 48} x)))
         0))
    48))

(defn base-note [bnote root beat]
  [(n bnote 0.125 0.3 0.35)
   (n root 0.125 0.2 0.35)])

(def main-synth* #_(wa/poly-synth wa/fm-synth {:attack 0.015 :sustain 0.1 :release 0.2 :amp 0.7 :modulation-index 20 :harmonicity 3} {})
  (wa/piano wa/ivy-audio-piano))

(defonce main-synth2*
  (wa/poly-synth wa/fm-synth { :sustain 0.1 :release 0.5 :amp 0.3 :modulation-index 40 :harmonicity 3 } {}))

(defonce main-synth3* (wa/poly-synth wa/fm-synth {:sustain 0.2 :release 0.3 :amp 0.4 :modulation-index 33 :harmonicity 3} {}))

#_(swap! music-state assoc
       :speed 0.37
       :start-time (sc/current-time* {:speed 0.31}))

(defcard bass-linee
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      main-synth*
      [:+
       (mapcat
        #(base-note %1 %2 %3)
        (mloop* [55 55 57 59 #_43])
        (mapcat #(take 8 (repeat %)) root-note)
        (beat 0.25))])))
  music-state)

(defcard impro-threeee
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      main-synth*
      [:+
      (map
       #(music-note scale %1 %2)
       (mapcat #(take 64 (repeat %)) root-note)
       (beat 0.25))])))
  music-state)

(defcard impro-high
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      main-synth2*
      [:+
       (map
        #(music-note-high scale %1 %2)
        (mapcat
         #(take 64 (repeat %))
         root-note)
        (beat 0.25))])))
  music-state)

(defonce bass-synth
    (wa/fm-synth {:attack 0.015 :sustain 0.1 :release 0.2 :amp 0.4 :modulation-index 20 :harmonicity 3}))

(defcard low-base-line
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      bass-synth
      [:+
       (map
        #(n (- %1 12) %2 0.4 0.2)
        (mapcat #(take 6 (repeat %)) root-note)
        (mloop* [(+ 0.25 0.125) 0.25 (- 0.5 0.125)]))])))
  music-state)

(defonce hh-synth2  (wa/sampler {44 "audio/505/hh.mp3" } {:amp 0.1 :spike 0.05}))
(def hh-synth (wa/noise {:amp 0.04
                         :spike 0.01
                         :decay 0.02
                         :attack 0.001
                         :sustain 0.01
                         :release 0.01 }))

(defcard kick
  (fn [da o]
   #_(play-music!
     @da
     (use-synth
      hh-synth2
      [:+
       (mapcat
        (fn [b]
          [(n 44 0.0625 0.3 0.01)
           (n 44 (- 0.25 0.0625) 0.2 0.1)])
        (beat 0.125))])))
  music-state)

(defcard hh
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      hh-synth
      [:+
       (map
        (fn [b]
          (n
           44
           0.0625
           (cosr b 0.5 0.5 (rand-nth [(/ 3 7) (/ 2 5)]))))
        (beat 0.125))])))
  music-state)



