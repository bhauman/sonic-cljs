(ns sonic-demos.sorensen
  (:require
   [devcards.core :as dc]
   [sonic-cljs.core :as sc :refer [n n* ch initial-player-state mloop music-root-card use-synth mloop* play-music!]]
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
          (+ (sc/note root) 12)
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
   (cosr beat 0.5 0.5 (/ 3 7))
   0.01))

(defn music-note-high [scale root beat]
  (n
   (right-hand-high scale root beat)
   (/ 0.25 4)
   (cosr beat 0.3 0.6 (/ 3 7))
   0.02))

(def root-note
  (iterate
    (fn [x]
      (- (rand-nth
          (seq (disj #{52 50 48} x)))
         0))
    48))

(defn base-note [bnote root beat]
  [(n bnote 0.125 0.8 0.7)
   (n root 0.125 0.5 0.7)])

(def main-synth*  (wa/piano wa/steinway-grand))
(def main-synth2* (wa/piano wa/steinway-grand))
(def main-synth3* (wa/piano wa/steinway-grand))

#_(swap! music-state assoc :start-time (sc/current-time* {:speed 0.31}))

(defcard bass-linee
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      main-synth*
      [:+
       (mapcat
        #(base-note %1 %2 %3)
        (mloop* (map #(- % 0) [55 55 57 59 #_43]))
        (mapcat #(take 8 (repeat %)) root-note)
        (beat 0.25))])))
  music-state)

(defcard impro-threeee
  (fn [da o]
    #_(play-music!
     @da
     (use-synth
      main-synth2*
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
      main-synth3*
      [:+
       (map
        #(music-note-high scale %1 %2)
        (mapcat
         #(take 64 (repeat %))
         root-note)
        (beat 0.125))])))
  music-state)

#_(defonce bass-synth
  (let [synth
        (js/Tone.FMSynth.
         #js {:harmonicity 2
              :modulationIndex 30
              :carrier #js   { :envelope #js {:release 1.3 }
                               :filterEnvelope #js {:release 1.3 } }
              :modulator #js { :envelope #js {:release 1.3 }
                              :filterEnvelope #js {:release 1.3 } }})]
    (.toMaster synth)))

#_(defcard low-base-line
  (fn [da o]
    (play-music!
     @da
     (use-synth
      bass-synth
      [:+
       (map
        #(n (- %1 12) %2 0.65 0.2)
        (mapcat #(take 6 (repeat %)) root-note)
        (mloop* [(+ 0.25 0.125) 0.25 (- 0.5 0.125)]))])))
  music-state)

(comment
  #js {:envelope
       #js {:sustain 0
            :attack 0.02
            :decay 0.8}
       :octaves 10})

(prn (p/find-note-name 42))

#_(defonce hh-synth
  (let [s
        (js/Tone.Sampler.
         #js {:hh "audio/505/hh.mp3" })]
    (.toMaster s)))

#_(defonce kick-synth
  (let [synth (js/Tone.NoiseSynth.)]
    (.toMaster synth)))

#_(defcard kick
  (fn [da o]
    (play-music!
     @da
     (use-synth
      kick-synth
      [:+
       (mapcat
        (fn [b]
          [(n 1 0.0625 0.15 0.01)
           (n 1 (- 0.25 0.0625) 0.07 0.1)])
        (beat 0.125))])))
  music-state)

#_(defcard hh
  (fn [da o]
    (play-music!
     @da
     (use-synth
      hh-synth
      [:+
       (map
        (fn [b]
          (n*
           "hh"
           0.0625
           (cosr b 0.25 0.25 (rand-nth [(/ 3 7) (/ 2 5)]))))
        (beat 0.25))])))
  music-state)


