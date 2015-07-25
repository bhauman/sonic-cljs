(ns sonic-cljs.webaudio
  (:require
   [sonic-cljs.pitch :as p]
   [sonic-cljs.trig :refer [cosr quantize]]
   [cljs.core.async :refer [<! timeout chan put! close!]]
   [clojure.string :as string])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [devcards.core :refer [defcard]]))

(defn l [arg]
  (.log js/console arg))

(defprotocol IPlayable
  (-play-note [this note-event]))

(defprotocol IDuration
  (-duration [this note-event]))

(defprotocol IDisposable
  (-dispose [this]))

(defonce ^:dynamic *context* (js/AudioContext.))

(defonce ^:dynamic *output*  (let [ gain (.createGain *context*)
                                   comp (.createDynamicsCompressor *context*)]
                               (.connect comp (.-destination *context*))
                               (set! (.. gain -gain -value) 0.95)
                               comp))

(defn to-master [node]
  (.connect node *output*))

;; envelope

(defn adsr-duration [{:keys [attack decay sustain release] :as env}]
  #_(prn env)
  (+ attack decay sustain release))

(defn ADSR-param [{:keys [attack decay sustain release amp spike]} param start-time]
  #_(prn [(+ amp spike ) amp])
  (doto param
    (.linearRampToValueAtTime 0   start-time)    
    (.linearRampToValueAtTime     (+ amp spike) #_(min 0.99 )
                                  (+ start-time attack))
    (.linearRampToValueAtTime amp (+ start-time attack decay))
    (.cancelScheduledValues   (+ start-time attack decay sustain))
    (.setValueAtTime amp          (+ start-time attack decay sustain))
    (.linearRampToValueAtTime 0   (+ start-time attack decay sustain release))))




;; sample loading

(defn load-sound [named-url]
  (let [out (chan)
        req (js/XMLHttpRequest.)]
    (set! (.-responseType req) "arraybuffer")
    (set! (.-onload req) (fn [e]
                           (if (= (.-status req) 200)
                             (do (put! out (assoc named-url :buffer (.-response req)) )
                                 (close! out))
                             (close! out))))
    (.open req "GET" (:url named-url) true)
    (.send req)
    out))

(defn decode [named-url]
  (let [out (chan)]
    (if (:buffer named-url)
      (do
        (.decodeAudioData
         *context* (:buffer named-url)
         (fn [decoded-buffer]
           (put! out (assoc named-url :decoded-buffer decoded-buffer))
           (close! out))
         (fn []
           (.error js/console "Error loading file " (prn named-url))
           (close! out))))
      (close! out))
    out))

(defn buffer-source [buffer]
  (let [source (.createBufferSource *context*)]
    (set! (.-buffer source) buffer)
    source))

(defn get-and-decode [named-url]
  (go
    (when-let [s (<! (load-sound named-url))]
      (<! (decode s)))))

(def steinway-note-names
  '(
    A0.mp3
    A6.mp3  Ab5.mp3	B3.mp3	Bb2.mp3	C1.mp3	C7.mp3	D5.mp3	Db4.mp3	E3.mp3	Eb2.mp3	F1.mp3	F7.mp3	G6.mp3	Gb5.mp3
    A1.mp3  A7.mp3	Ab6.mp3	B4.mp3	Bb3.mp3	C2.mp3	C8.mp3	D6.mp3	Db5.mp3	E4.mp3	Eb3.mp3	F2.mp3	G1.mp3	G7.mp3	Gb6.mp3
    A2.mp3  Ab1.mp3	Ab7.mp3	B6.mp3	Bb4.mp3	C3.mp3	D1.mp3	D7.mp3	Db6.mp3	E5.mp3	Eb4.mp3	F3.mp3	G2.mp3	Gb1.mp3	Gb7.mp3
    A3.mp3  Ab2.mp3	B0.mp3	B7.mp3	Bb5.mp3	C4.mp3	D2.mp3	Db1.mp3	Db7.mp3	E6.mp3	Eb5.mp3	F4.mp3	G3.mp3	Gb2.mp3
    A4.mp3  Ab3.mp3	B1.mp3	Bb0.mp3	Bb6.mp3	C5.mp3	D3.mp3	Db2.mp3	E1.mp3	E7.mp3	Eb6.mp3	F5.mp3	G4.mp3	Gb3.mp3
    A5.mp3  Ab4.mp3	B2.mp3	Bb1.mp3	Bb7.mp3	C6.mp3	D4.mp3	Db3.mp3	E2.mp3	Eb1.mp3	Eb7.mp3	F6.mp3	G5.mp3	Gb4.mp3))

(defn steinway-name-to-pitch [n]
  (p/note (first (string/split n "."))))

(defn load-steinway-grand []
  (go-loop [result {}
            nns steinway-note-names]
    (if-not (nil? (first nns))
      (let [file-name (name (first nns))
            pitch (steinway-name-to-pitch file-name)
            decoded-buffer (<! (get-and-decode {:url (str "audio/SteinwayGrand/" file-name)
                                                :pitch pitch}))]
        (prn file-name)
        (prn pitch)
        (prn decoded-buffer)
        (recur (assoc result pitch decoded-buffer)
               (rest nns)))
      result)))

(defn num-to-name [i]
  (if (< i 10)
    (str "0" i)
    (str i)))

(defn load-ivy-audio-piano []
  (go-loop [result {}
            nns (range 1 89)(map #(if (= 1 (count %)) (str "0" %) %) (map str ))]
    (if-not (nil? (first nns))
      (let [file-name (num-to-name (first nns))
            pitch (first nns)
            decoded-buffer (<! (get-and-decode {:url (str "audio/ivy-audio/" file-name ".mp3")
                                                :pitch pitch}))]
        (prn file-name)
        (prn pitch)
        (prn decoded-buffer)
        (recur (assoc result pitch decoded-buffer)
               (rest nns)))
      result)))


(defonce load-grand
  (go
    (def steinway-grand (<! (load-steinway-grand)))
    (prn "Steinway grand loaded")))

(defonce loading-ivy-audio-piano
  (go
    (def ivy-audio-piano (<! (load-ivy-audio-piano)))
    (prn "Ivy audio grand loaded")))

#_(prn steinway-grand)


#_(prn (map scale-gain-to-frequency [1 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.02]))

(defn filter-frequency [{:keys [velocity pitch] :as n}]
  (let [velocity (min (max velocity 0.000000001) 1)
        f (+ (* 3500 velocity)
             (* (/ 1 velocity) 50))]
    #_(prn n)
    f))

(defn piano [decoded-buffers]
  (prn "Creating Piano")
  (let [output (.createGain *context*)
        delay  (.createDelay *context*)
        delay-gain (.createGain *context*)
        create-filter (fn []
                        (let [gain (.createGain *context*)
                              filt (.createBiquadFilter *context*)]
                          (set! (.. filt -type) "lowpass")
                          (.connect filt gain)
                          (.connect gain output)
                          (.connect gain delay)
                          {:gain gain :filt filt}))
        filter-bank (mapv (fn [_] (create-filter)) (range 10))
        ^:mutable end-times {}
        get-filter (fn [end-times]
                     (first (sort-by (fn [i] (or (get-in end-times [i :end-time]) 0)) 
                                     filter-bank)))
        
        get-filter2 (fn [_]
                      (create-filter)
                      )
        default-envelope {:attack 0.1
                          :decay 0.1
                          :sustain 1.3
                          :release 0.2
                          :amp 0.9
                          :spike 0.1}]
    (set! (.. delay -delayTime -value) 0.0001)
    (set! (.. output -gain -value) 0.95)
    
    (set! (.. delay-gain -gain -value) 0.4)
    (.connect delay delay-gain)
    (.connect delay-gain output)
    (.connect output *output*)
    (reify
      IPlayable
      (-play-note [_ note-event]
        (let [abs-start-time (or (:abs-start-time note-event)
                                 (+ 0.1 (.-currentTime *context*)))
              end-time (+ abs-start-time 4)
              envelope (merge default-envelope
                              (select-keys note-event [:sustain
                                                       :decay
                                                       :attack
                                                       :release
                                                       :amp
                                                       :spike]))
              velocity (min (max (:velocity note-event) 0) 1)
              envelope (assoc envelope :amp (* velocity (:amp envelope)))]
          (when (> velocity 0.001)
            (if-let [avail-filt (get-filter2 end-times)]
              (if-let [buffer (get decoded-buffers (:pitch note-event))]
                (do
                  #_(prn (adsr-duration envelope))
                  #_(prn (map (fn [i] (get end-times i)) (sort-by (fn [i] (or (get end-times i) 0)) filter-bank)))
                  #_(prn buffer)
                  #_(prn "playing")
                  #_(prn (hash avail-filt))
                  (let [;last-source  (get-in end-times [avail-filt :source])
                        source (buffer-source (:decoded-buffer buffer))]
                    (set! end-times (assoc end-times avail-filt {:end-time end-time
                                                                 :source   source}))
                    
                    (ADSR-param envelope (.. (:gain avail-filt) -gain) abs-start-time)
                    #_(.setValueAtTime (.. (:gain avail-filt) -gain)
                                       (* 0.85 gain-val)
                                       abs-start-time)
                    (.setValueAtTime (.. (:filt avail-filt) -frequency) (filter-frequency note-event) abs-start-time)
                    (.connect source (:filt avail-filt))
                    #_(when last-source
                        (prn "Stopping")
                        
                        (.stop  last-source (+ abs-start-time 0.001)))
                    (.start source abs-start-time)
                    (.stop source (+ abs-start-time (adsr-duration envelope) 0.3)))))
              (.error js/console "Not playing note as no filter is available."))
            )))
      IDuration
      (-duration [_ note-event] 4))))

(comment
  (def piano-sampler (piano steinway-grand))


(-play-note piano-sampler {:pitch 55 :velocity 1   :abs-start-time (+ 1 (.-currentTime *context*)) })
(-play-note piano-sampler {:pitch 55 :velocity 0.7 :abs-start-time (+ 3 (.-currentTime *context*)) })
(-play-note piano-sampler {:pitch 55 :velocity 0.5 :abs-start-time (+ 5 (.-currentTime *context*)) })
(-play-note piano-sampler {:pitch 55 :velocity 0.3 :abs-start-time (+ 7 (.-currentTime *context*)) })
(-play-note piano-sampler {:pitch 55 :velocity 0.1 :abs-start-time (+ 9 (.-currentTime *context*)) }))



(defn create-osc-gain []
  (let [osc (.createOscillator *context*)
        gain (.createGain *context*)]
    (.connect osc gain)
    (set! (.. osc -type) "sine")
    (set! (.. gain -gain -value) 0)
    (.start osc)
    (to-master gain)
    {:osc  osc
     :gain gain
     :attack    0.01
     :decay     0.0     
     :sustain   0.2
     :release   0.3
     :spike     0.1
     :amp       0.3}))



(defn oscillator [options]
  (let [osc (.createOscillator *context*)
        gain (.createGain *context*)
        filter (.createBiquadFilter *context*)]
    (.connect osc gain)
    (.connect gain filter)
    (set! (.. filter -type) "lowpass")
    (set! (.. filter -frequency -value) 350)
    (set! (.. filter -frequency -Q) 6)    
    (set! (.. osc -type) "square")
    (set! (.. gain -gain -value) 0)
    (.start osc)
    (to-master gain)
    (specify
        {:osc  osc
         :gain gain
         :options
         (atom (merge {:attack    0.01
                       :decay     0.0     
                       :sustain   0.2
                       :release   0.3
                       :amp       0.9
                       :spike     0.1}
                      options))}
           IPlayable 
           (-play-note [this {:keys [pitch duration abs-start-time sustain velocity] :as note-event}] 
             (let [abs-start-time    (or abs-start-time (+ 0.1 (.-currentTime *context*)))
                   gain-param        (.. (:gain this) -gain)
                   osc-freq-param    (.. (:osc this)  -frequency)
                   options           (merge @(:options this) (select-keys note-event [:sustain
                                                                                      :decay
                                                                                      :attack
                                                                                      :release
                                                                                      :amp
                                                                                      :spike]))
                   options          (if velocity (assoc options :amp (* (:amp options) (max velocity 1)))
                                         options)]
               (.setValueAtTime osc-freq-param (p/midi->hz pitch) abs-start-time)
               (ADSR-param
                options
                gain-param abs-start-time)))
           IDuration 
           (-duration [this note-event]
             (adsr-duration (merge @(:options this)
                                   (select-keys note-event [:sustain
                                                            :decay
                                                            :attack
                                                            :release
                                                            :amp
                                                            :spike])))))))



(def multipliyer 2)
(def detune (* 0 (js/Math.pow 2 (/ 1 12))))

#_(prn detune)

(defn fm-synth [options]
  (let [modulator (.createOscillator *context*)
        mod-gain  (.createGain *context*)
        carrier   (.createOscillator *context*)
        car-gain  (.createGain *context*)
        filter (.createBiquadFilter *context*)]

    (set! (.. filter -type) "lowpass")
    (set! (.. filter -frequency -value) 350)
    (set! (.. filter -frequency -Q) 6)    
    
    (set! (.. modulator -type) "sine")
    (.connect modulator mod-gain)
    (set! (.. mod-gain -gain -value) 300)
    
    (set! (.. modulator -detune -value) detune)
    (set! (.. carrier -type) "square")
    (.connect carrier car-gain)
    (set! (.. car-gain -gain -value) 0)

    (.connect car-gain filter)    
    (.connect mod-gain (.-frequency carrier))
    (.start modulator)
    (.start carrier)
    (to-master filter)
    (specify
        {
         :options
         (atom (merge {:carrier
                       {:attack    0.01
                        :decay     0.01     
                        :sustain   0.2
                        :release   0.3
                        :amp       0.6
                        :spike     0.1}
                       :modulator
                       {:attack    0.0
                        :decay     0.0     
                        :sustain   0.2
                        :release   0.3
                        :amp       300
                        :spike     0.1}
                       }
                      options))}

           IPlayable 
           (-play-note [this {:keys [pitch duration abs-start-time sustain velocity] :as note-event}] 
             (let [abs-start-time    (or abs-start-time (+ 0.1 (.-currentTime *context*)))
                   car-gain-param        (.. car-gain -gain)
                   carrier-freq-param    (.. carrier  -frequency)
                   mod-freq-param        (.. modulator  -frequency)
                   mod-gain-param        (.. mod-gain  -gain)                   
                   car-options           (merge (:carrier @(:options this))
                                                (select-keys note-event [:sustain
                                                                         :decay
                                                                         :attack
                                                                         :release
                                                                         :amp
                                                                         :spike]))
                   car-options          (if velocity (assoc car-options :amp
                                                            (* (:amp car-options) (min velocity 1)))
                                        car-options)]
               (.setValueAtTime carrier-freq-param (p/midi->hz pitch) abs-start-time)
               (.setValueAtTime mod-freq-param (* (p/midi->hz pitch) multipliyer)
                                abs-start-time)
               (ADSR-param
                car-options
                car-gain-param abs-start-time)
               #_(ADSR-param
                (:modulator @(:options this))
                mod-gain-param abs-start-time)))

           IDuration 
           (-duration [this note-event]
             (adsr-duration (merge (:carrier @(:options this))
                                   (select-keys note-event [:sustain
                                                            :decay
                                                            :attack
                                                            :release
                                                            :amp
                                                            :spike])))))))

#_(def fm (fm-synth {}))
#_(-play-note fm {:pitch 55
                :velocity 0.3
                :abs-start-time (+ 0.1 (.-currentTime *context*))})

(defn poly-synth [synth-constructor synth-options options]
  (let [insts      (mapv (fn [_] (synth-constructor synth-options)) (range (or (:voices options)
                                                                              5)))
        ^:mutable end-times {}]
    (reify
      IPlayable
      (-play-note [_ note-event]
        (let [abs-start-time (or (:abs-start-time note-event)
                              (+ 0.1 (.-currentTime *context*)))
              end-time (+ abs-start-time
                          (-duration (first insts) note-event))]
          (when-let [avail-inst
                     (first (filter (fn [i] (> abs-start-time (+ (get end-times i) 0.2)))
                                    insts))]
            #_(prn (hash avail-inst))
            (set! end-times (assoc end-times avail-inst end-time))
            (-play-note avail-inst (assoc note-event :abs-start-time abs-start-time))
            )))
      IDuration
      (-duration [_ note-event]
        (-duration (first insts) note-event)))))

#_(defonce poly (poly-synth fm-synth {} {}))

(comment

  (comment
  
(-play-note poly {:pitch 55
                  :abs-start-time (+ 0.1 (.-currentTime *context*)) })

(-play-note poly {:pitch 57 :abs-start-time (+ 0.2 (.-currentTime *context*)) })
(-play-note poly {:pitch 59 :abs-start-time (+ 0.3 (.-currentTime *context*)) })
(-play-note poly {:pitch 57 :abs-start-time (+ 0.4 (.-currentTime *context*)) })
(-play-note poly {:pitch 59 :abs-start-time (+ 0.5 (.-currentTime *context*)) })

(-play-note poly {:pitch 55
                  :abs-start-time (+ 0.6 (.-currentTime *context*)) })
(-play-note poly {:pitch 57 :abs-start-time (+ 0.7 (.-currentTime *context*)) })
(-play-note poly {:pitch 59 :abs-start-time (+ 0.8 (.-currentTime *context*)) })
(-play-note poly {:pitch 57 :abs-start-time (+ 0.9 (.-currentTime *context*)) })
(-play-note poly {:pitch 59 :abs-start-time (+ 0.10 (.-currentTime *context*)) })

)




#_(def osc (oscillator {}))
#_(-play-note osc {:pitch 55 :abs-start-time (+ 0.1 (.-currentTime *context*))
                 :sustain 0.1
                 :velocity 1})


(def scale (p/scale-field :E :aeolian))


(defn right-hand-n [root beat]
  (quantize
   (int
    (cosr beat (cosr beat 3 5 2)
          (+ root #_(sc/note root)
             24)
          (/ 3 7))) scale))

(defn right-hand-high [root beat]
  (if (< (rand) 0.6)
    (quantize
     (+ 7
        (int
         (cosr beat (cosr beat 3 5 2)
               (+ root #_(sc/note root)
                  24)
               (/ 3 7))))
     scale)
    nil))

(def root-note
  (iterate
    (fn [x]
      (rand-nth
       (seq (disj #{52 50 48} x))))
    48))


(prn (take 1 root-note))


(def melody (map
              right-hand-n
              (mapcat (fn [x] [x x x x x x x x]) root-note)
              (iterate inc 0.25)))

(prn (take 10 melody))

(def timer  125)

(prn (rand))

(defn looper [synth]
  (go-loop [s melody]
    (-play-note synth {:pitch (first s) :sustain 0.01
                       :velocity (+ 0.4 (* 0.6 (rand)))
                       :abs-start-time (+ 0.75 (.-currentTime *context*)) })
    (<! (timeout (#(do timer))))
    (recur (rest s))))





#_(def looping (looper  (piano steinway-grand) #_(poly-synth oscillator {} {:voices 6})))



#_(def looping2 (looper  (piano ivy-audio-piano) #_(poly-synth oscillator {} {:voices 6})))
#_(def looping3 (looper  (piano ivy-audio-piano) #_(poly-synth oscillator {} {:voices 6})))
#_(def looping4 (looper  (piano ivy-audio-piano) #_(poly-synth oscillator {} {:voices 6})))

  )









