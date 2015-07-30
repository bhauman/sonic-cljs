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

(defprotocol IOptionsSchema
  (-options-schema   [this]))

(defonce ^:dynamic *context* (js/AudioContext.))

(defonce ^:dynamic *output*  (let [ gain (.createGain *context*)
                                    comp (.createDynamicsCompressor *context*)]
                               (.connect gain (.-destination *context*))
                               (.connect comp gain)
                               (set! (.. gain -gain -value) 0.9)
                               comp))

(defn to-master [node]
  (.connect node *output*))

;; envelope

(defn adsr-duration [{:keys [attack decay sustain release] :as env}]
  #_(prn env)
  (+ attack decay sustain release))

(def envelope-params [:attack-level :sustain-level :attack :decay :sustain :release :amp :velocity :min])

;; inspired by the sonic pi parameters and defaults
(defn ADSR-param [{:keys [attack-level sustain-level attack decay sustain release amp velocity min]
                   :or   {attack-level 1
                          sustain-level 1
                          attack 0
                          decay 0
                          sustain 0
                          release 1
                          amp 1
                          velocity 1
                          min 0}}
                  param
                  start-time]
  (let [velocity      (max (cljs.core/min velocity 1) 0)
        bottom-gain   min
        sustain-gain  (+ bottom-gain (* amp sustain-level velocity))
        attack-gain   (+ bottom-gain (* amp attack-level velocity))]
    (doto param
      (.cancelScheduledValues                start-time)
      (.setValueAtTime          bottom-gain  start-time)    
      (.linearRampToValueAtTime attack-gain  (+ start-time attack))
      (.linearRampToValueAtTime sustain-gain (+ start-time attack decay))
      (.cancelScheduledValues                (+ start-time attack decay sustain))
      (.setValueAtTime          sustain-gain (+ start-time attack decay sustain))
      (.linearRampToValueAtTime bottom-gain  (+ start-time attack decay sustain release)))))

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
            decoded-buffer (<! (get-and-decode {:url (str "audio/SteinwayGrandSmaller/" file-name)
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
      (let [file-name (num-to-name (first nns)) ;; its 32 off
            pitch (+ (first nns) 20)
            decoded-buffer (<! (get-and-decode {:url (str "audio/ivy-audio/" file-name ".mp3")
                                                :pitch pitch}))]
        (prn file-name)
        (prn pitch)
        (prn decoded-buffer)
        (recur (assoc result pitch decoded-buffer)
               (rest nns)))
      result)))

#_(defonce load-grand
  (go
    (def steinway-grand (<! (load-steinway-grand)))
    (prn "Steinway grand loaded")))

(defonce loading-ivy-audio-piano
  (go
    (def ivy-audio-piano (<! (load-ivy-audio-piano)))
    (prn "Ivy audio grand loaded")))

(defn merge-envelope [default-envelope envelope-like]
  (merge default-envelope (select-keys envelope-like envelope-params)))

(defn start-time [note-event]
  (or (:abs-start-time note-event) (+ 0.1 (.-currentTime *context*))))

(defn valid-envelope? [envelope]
  (> (:velocity envelope) 0.001))

;; this is linear could be much better
(defn filter-frequency [{:keys [velocity pitch] :as n}]
  (-> velocity
    (max 0.00000001)
    (min 1)
    (* 2)
    (* 2500))) ;; middle frequency

(defn piano [decoded-buffers]
  (let [output     (.createGain *context*)
        delay      (.createDelay *context*)
        delay-gain (.createGain *context*)
        create-filter (fn []
                        (let [gain (.createGain *context*)
                              filt (.createBiquadFilter *context*)]
                          (set! (.. filt -type) "lowpass")
                          (.connect filt gain)
                          (.connect gain output)
                          (.connect gain delay)
                          {:gain gain :filt filt}))
        default-envelope {:attack 0.01
                          :decay 0.1
                          :sustain 1.3
                          :release 0.2
                          :amp 0.9
                          :sustain-level 0.9}]
    
    (set! (.. delay -delayTime -value) 0.0001)
    (set! (.. output -gain -value) 0.95)
    
    (set! (.. delay-gain -gain -value) 0.4)
    (.connect delay delay-gain)
    (.connect delay-gain output)
    (.connect output *output*)
    (reify
      IPlayable
      (-play-note [_ note-event]
        (let [abs-start-time (start-time note-event)
              envelope       (merge-envelope default-envelope note-event)]
          (when (valid-envelope? envelope)
            (when-let [buffer  (get decoded-buffers (:pitch note-event))]
              (let [source     (buffer-source (:decoded-buffer buffer))
                    avail-filt (create-filter)]
                (ADSR-param envelope (.. (:gain avail-filt) -gain) abs-start-time)
                (.setValueAtTime (.. (:filt avail-filt) -frequency) (filter-frequency note-event) abs-start-time)
                (.connect source (:filt avail-filt))
                (.start source abs-start-time)
                (.stop source (+ abs-start-time (adsr-duration envelope) 0.1)))))))
      IDuration
      (-duration [_ note-event]
        (adsr-duration (merge-envelope default-envelope note-event))))))


(comment
  (def piano-sampler (piano ivy-audio-piano))

  (-play-note piano-sampler {:pitch 55 :velocity 1   :abs-start-time (+ 1 (.-currentTime *context*)) })
  (-play-note piano-sampler {:pitch 55 :velocity 0.7 :abs-start-time (+ 3 (.-currentTime *context*)) })
  (-play-note piano-sampler {:pitch 55 :velocity 0.5 :abs-start-time (+ 5 (.-currentTime *context*)) })
  (-play-note piano-sampler {:pitch 55 :velocity 0.3 :abs-start-time (+ 7 (.-currentTime *context*)) })
  (-play-note piano-sampler {:pitch 55 :velocity 0.1 :abs-start-time (+ 9 (.-currentTime *context*)) }))

(defn buffer-map-player [buffer-map-atom options]
  (let [output (.createGain *context*)
        create-gain (fn []
                      (let [gain (.createGain *context*)]
                        (.connect gain output)
                        gain))
        default-envelope (merge {:sustain-level 0.9
                                 :attack 0.01
                                 :decay 0.1
                                 :sustain 1.3
                                 :release 0.2
                                 :velocity 1
                                 :amp 0.9}
                                options)]
    (.connect output *output*)
    (reify
      IPlayable
      (-play-note [_ note-event]
        (let [abs-start-time (or (:abs-start-time note-event) (+ 0.1 (.-currentTime *context*)))
              envelope (merge-envelope default-envelope note-event)]
          (when (valid-envelope? envelope)
            (when-let [buffer
                       (if (= 1 (count @buffer-map-atom))
                         (second (first @buffer-map-atom))
                         (get @buffer-map-atom (:pitch note-event)))]
              (when-let [source (buffer-source buffer)]
                (let [gain (create-gain)]
                  (when (:loop default-envelope)
                    (set! (.-loop source) true))
                  (.connect source gain) 
                  (ADSR-param envelope (.. gain -gain) abs-start-time)
                  (.start source abs-start-time)
                  (.stop source (+ abs-start-time (adsr-duration envelope) 0.1))))))))
      IDuration
      (-duration [_ note-event]
        (adsr-duration
         (merge default-envelope
                (select-keys note-event [:sustain
                                         :decay
                                         :attack
                                         :release
                                         :amp
                                         :spike])))))))

(defn sampler [map-of-midi-to-urls options]
  (let [buffers-atom (atom {})]
    (go-loop [urls  map-of-midi-to-urls]
      (when-let [[pitch url] (first urls)]
        (let [d (<! (get-and-decode {:url url
                                     :pitch pitch}))]

          (when-let [{:keys [pitch] :as decoded} d]
            (swap! buffers-atom assoc pitch (:decoded-buffer decoded)))
          (recur (rest urls)))))
    (buffer-map-player buffers-atom options)))

#_(def hh (sampler {44 "audio/505/hh.mp3" } {}))

#_(js/setTimeout #(-play-note hh {:pitch 44
                :velocity 1
                :release 2
                                :abs-start-time (+ 1 (.-currentTime *context*)) })
               300)

(defn mono-synth [options]
  (let [osc (.createOscillator *context*)
        gain (.createGain *context*)
        filter (.createBiquadFilter *context*)]
    (.connect osc filter)
    (.connect filter gain)
    (set! (.. filter -type) "lowpass")
    (set! (.. filter -frequency -Q) 6)
    (set! (.. osc -type) "square")
    (set! (.. gain -gain -value) 0)
    (.start osc)
    (to-master gain)
    (specify
        {:osc  osc
         :gain gain
         :filter filter
         :options
         (atom
          {:envelope
           {:attack    0.01
            :decay     0.2     
            :sustain   0.5 ;; this is sustain time
            :sustain-level 0.4 
            :release   0.5
            :amp       1}  
           :filter-envelope
           {:attack    0.06
            :decay     0.02     
            :sustain   0.5 ;; this is sustain time
            :sustain-level 0.5
            :release   1.0
            :min       (* 20 20)
            :amp       (* 2000 2000)}
           })}

           IPlayable 
           (-play-note [this {:keys [pitch duration abs-start-time sustain velocity] :as note-event}] 
             (let [abs-start-time    (start-time note-event)
                   gain-param          (.. gain -gain)
                   osc-freq-param      (.. osc  -frequency)
                   filter-freq-param    (.. filter  -frequency)
                   envelope           (merge-envelope
                                       (:envelope @(:options this))
                                       note-event)]
               ;; should probably use detune here
               (.setValueAtTime osc-freq-param (p/midi->hz pitch) abs-start-time)
               (ADSR-param
                options
                gain-param
                abs-start-time)
               (ADSR-param
                (:filter-envelope @(:options this))
                filter-freq-param abs-start-time)))
           IDuration 
           (-duration [this note-event]
             (adsr-duration (merge-envelope
                             (:envelope @(:options this))
                             note-event))))))

#_(def mono (mono-synth {}))
#_(-play-note mono {:pitch 55
                :velocity 1
                :abs-start-time (+ 0.1 (.-currentTime *context*))})




(defn envelope-options [options-atom]
  (letfn [(common [name max key]
            {:key key
             :ui-hint :slider
             :name name
             :min 0 :max max
             :precision 6
             :default-value (get-in @options-atom [:carrier key])
             :set! #(swap! options-atom update-in [:carrier key] (fn [] %))}
            )]
    [(common "Volume" 1 :amp)
     (common "Attack" 2 :attack)
     (common "Decay" 2 :decay)
     (common "Sustain" 2 :sustain)
     (common "Release" 2 :release)
     (common "Sustain Level" 1 :sustain-level)
     (common "Attack Level" 1 :attack-level)]))


(defn fm-synth [options]
  (let [modulator  (.createOscillator *context*)
        mod-gain   (.createGain *context*)
        carrier    (.createOscillator *context*)
        car-gain   (.createGain *context*)

        detune     (or (:detune options) 0)
        delay      (when (:delay options)
                     (.createDelay *context*))
        output    (.createGain *context*)

        modulation-index (or (:modulation-index options ) 40.2)
        harmonicity      (or (:harmonicity options ) 3)        
        carrier-envelope
        (merge-envelope {:attack    0.01
                         :decay     0.0     
                         :sustain   0.5
                         :release   0.4
                         :amp         1
                         :attack-level 1
                         :sustain-level 1}
                        options)
        modulator-envelope (assoc carrier-envelope :amp (* modulation-index modulation-index))]
    
    (set! (.. modulator -type) "triangle")
    ;; (.connect modulator mod-filter)
    (.connect modulator mod-gain)
    (set! (.. mod-gain -gain -value) (* modulation-index modulation-index))
    
    (set! (.. modulator -detune -value) detune)
    (set! (.. carrier -type) "sine")
    (.connect carrier car-gain)
    (set! (.. car-gain -gain -value) 0)

    (.connect mod-gain (.-frequency carrier))
    (.connect car-gain output)

    (when delay
      (set! (.. delay -delayTime -value) 0.001)
      (.connect car-gain delay)
      (.connect delay output))
    
    (.connect car-gain output)
    (.start modulator)
    (.start carrier)
    (to-master output)
    (let [options-atom
          (atom (merge {:harmonicity harmonicity
                        :modulation-index modulation-index
                        :detune detune   
                        :carrier carrier-envelope
                        :modulator modulator-envelope}
                       (-> options
                         (dissoc :carrier)
                         (dissoc :modulator))))]
      (reify
        IOptionsSchema
        (-options-schema [this]
          (concat
           [{:key :harmonicity
             :ui-hint :slider
             :name "Harmonicity" :min 0 :max 10
             :default-value harmonicity
             :set! #(swap! options-atom assoc :harmonicity %)}
            {:key :modulation-index
             :ui-hint :slider
             :name "Modulation"
             :min 0 :max 100
             :default-value modulation-index
             :set! #(swap! options-atom assoc :modulation-index %)}]
           (envelope-options options-atom)))
        IPlayable 
        (-play-note [this {:keys [pitch duration abs-start-time sustain velocity] :as note-event}] 
          (let [options           @options-atom
                modulation-index  (:modulation-index options)
                harmonicity       (:harmonicity options)                   
                abs-start-time        (start-time note-event)
                car-gain-param        (.. car-gain -gain)
                carrier-freq-param    (.. carrier  -frequency)
                mod-freq-param        (.. modulator  -frequency)
                mod-gain-param        (.. mod-gain  -gain)                   
                car-options           (merge-envelope (:carrier options)
                                                      note-event)
                mod-options           (merge (:modulator options)
                                             {:amp (* modulation-index modulation-index)}
                                             (select-keys car-options
                                                          [:sustain
                                                           :decay
                                                           :attack
                                                           :release]))]
            (.setValueAtTime carrier-freq-param (p/midi->hz pitch) abs-start-time)
            (.setValueAtTime mod-freq-param     (* (p/midi->hz pitch)
                                                   harmonicity)
                             abs-start-time)
            (ADSR-param
             car-options
             car-gain-param abs-start-time)
            (ADSR-param
             mod-options
             mod-gain-param abs-start-time)))
      
        IDuration 
        (-duration [this note-event]
          (adsr-duration (merge-envelope (:carrier @options-atom)
                                         note-event)))))))

(comment
  (def fsynth (fm-synth {:modulation-index 10 :harmonicity 3}))

  (-play-note fsynth {:pitch 65 :velocity 1   :abs-start-time (+ 1 (.-currentTime *context*)) })
  #_(-play-note fsynth {:pitch 65 :velocity 0.7 :abs-start-time (+ 3 (.-currentTime *context*)) })
  #_(-play-note fsynth {:pitch 65 :velocity 0.5 :abs-start-time (+ 5 (.-currentTime *context*)) })
  #_(-play-note fsynth {:pitch 65 :velocity 0.3 :abs-start-time (+ 7 (.-currentTime *context*)) })
  #_(-play-note fsynth {:pitch 65 :velocity 0.1 :abs-start-time (+ 9 (.-currentTime *context*)) })
  )


#_(def fm (fm-synth {}))
#_(-play-note fm {:pitch 55
                :velocity 0.3
                :abs-start-time (+ 0.1 (.-currentTime *context*))})

(defn get-option [key options]
  (first (filter #(= (:key %) key) options)))

(defn proxy-schema [insts]
  (map 
   (fn [option]
     (assoc
      option
      :set!
      (fn [v]
        (doseq [opt (mapv #(get-option (:key option) (-options-schema %))
                          insts)]
          ((:set! opt) v)))))
   (-options-schema (first insts))))

(defn poly-synth [synth-constructor synth-options options]
  (let [insts      (mapv (fn [_] (synth-constructor synth-options)) (range (or (:voices options)
                                                                              5)))
        ^:mutable end-times {}]
    (reify
      IOptionsSchema
      (-options-schema [this] (proxy-schema insts))
      IPlayable
      (-play-note [_ note-event]
        (let [abs-start-time (or (:abs-start-time note-event)
                              (+ 0.1 (.-currentTime *context*)))
              end-time (+ abs-start-time
                          (-duration (first insts) note-event))]
          (when-let [avail-inst
                     (first (filter (fn [i] (> abs-start-time
                                              (+ (get end-times (hash i)) 0.25)))
                                    insts))]
            (set! end-times (assoc end-times
                                   (hash avail-inst)
                                   end-time))
            (-play-note avail-inst (assoc note-event :abs-start-time abs-start-time))
            )))
      IDuration
      (-duration [_ note-event]
        (-duration (first insts) note-event)))))

(comment
  (def poly (poly-synth fm-synth {:modulation-index 53 :harmonicity 3 :sustain 0.1 :release 0.1 :amp 0.5} {}))
  (-play-note poly {:pitch 55
                    :abs-start-time (+ 0.1 (.-currentTime *context*)) })
  
  (-play-note poly {:pitch 57 :abs-start-time (+ 0.2 (.-currentTime *context*)) })
  (-play-note poly {:pitch 59 :abs-start-time (+ 0.3 (.-currentTime *context*)) })
  (-play-note poly {:pitch 57 :abs-start-time (+ 0.4 (.-currentTime *context*)) })
  (-play-note poly {:pitch 59 :abs-start-time (+ 0.5 (.-currentTime *context*)) })
  
  (-play-note poly {:pitch 55 :abs-start-time (+ 0.6 (.-currentTime *context*)) })
  (-play-note poly {:pitch 57 :abs-start-time (+ 0.7 (.-currentTime *context*)) })
  (-play-note poly {:pitch 59 :abs-start-time (+ 0.8 (.-currentTime *context*)) })
  (-play-note poly {:pitch 57 :abs-start-time (+ 0.9 (.-currentTime *context*)) })
  (-play-note poly {:pitch 59 :abs-start-time (+ 0.10 (.-currentTime *context*)) })
  
)

(def noise-buffer
  (let [buffer (.createBuffer *context* 1
                              22050
                              (.-sampleRate *context*))
        data   (.getChannelData buffer 0)]
    (doseq [i (range 22050)]
      (aset data i (- (* 2 (js/Math.random)) 1)))
    buffer))

#_(let [b (buffer-source noise-buffer)]
  (.connect b *output*)
  #_(set! (.-loop b) true)
  (.start b)
  (js/setTimeout (fn [] (.stop b)) 1000))

(defn noise [options]
  (buffer-map-player
   (atom {44 noise-buffer})
   (assoc options :loop true)))

(do
  (def noisey (noise {:amp 0.1
                      :sustain-level 0.3
                      :attack 0.01
                      :decay 0.01
                      :sustain 0.04
                      :release 0.05}))

  
  (-play-note noisey {:pitch 55
                      :velocity 0.7
                      :abs-start-time (+ 0.1 (.-currentTime *context*)) }))








