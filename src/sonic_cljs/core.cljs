(ns sonic-cljs.core
  (:require
   #_[om.core :as om :include-macrosclude-macros true]
   [sablono.core :as sab :include-macros true]
   [sonic-cljs.trig :refer [cosr sinr]]
   [sonic-cljs.pitch :as p]   
   [cljs.test :as t :include-macros true :refer-macros [is testing]]
   [cljs.core.async :as ca :refer [<! chan timeout]])
  (:require-macros
   [devcards.core :as dc :refer [defcard defcard-doc deftest]]
   [cljs-react-reload.core :refer [defonce-react-class def-react-class]]
   [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(def default-play-speed 0.41)

(defn fm-synth []
  (prn "Creating fm synth")
  (let [synth
        (js/Tone.PolySynth.
         4
         js/Tone.FMSynth
         #js {:harmonicity 3
              :modulationIndex 7
              :carrier #js { :envelope #js {:release 1.3 }
                             :filterEnvelope #js {:release 1.3 } }
              :modulator #js { :envelope #js {:release 1.3 }
                               :filterEnvelope #js {:release 1.3 } }})]
    (.toMaster synth)
     synth))

(defonce reload-atom
  (do
    (.addEventListener js/document.body "figwheel.js-reload"
                       (fn [e] (swap! reload-atom inc)))
    (atom 0)))

;;document.body.addEventListener("figwheel.js-reload", function (e) {console.log(e.detail);} );

(defonce default-synth (atom (fm-synth)))

(defonce default-synth-watch
  (do
    (add-watch default-synth :synth-change-watch
               (fn [_ _ o n]
                 (.dispose o)
                 (.toMaster n)))
    true))

(dc/do ;; this will only be created in devcards context
  (defn start-stop-button [data-atom owner]
    (when (:__start-stop-button @data-atom)
      (js/requestAnimationFrame #(.forceUpdate owner)))
    [:button {:onClick #(swap! data-atom update-in [:__start-stop-button] 
                               not)} "start stop"]))

(defn add-time [t1 t2]
  (if (or (= t1 :inf) (= t2 :inf))
    :inf
    (+ t1 t2)))

(defn subtract-time [t1 t2]
  (if (or (= t1 :inf) (= t2 :inf))
    :inf
    (- t1 t2)))

(defn max-time [t1 t2]
  (if (or (= t1 :inf) (= t2 :inf))
    :inf
    (if (> t1 t2) t1 t2)))

(defmulti process-music (fn [start-time t]
                          (cond
                            (vector? t) (first t)
                            (map?    t) :durationable)))



(defmethod process-music :durationable [start-time {:keys [duration] :as durationable}]
  (assoc durationable
         :start-time start-time
         :end-time (add-time start-time (:duration durationable))))

(defmethod process-music :+ [start-time [_ durationables]]
  (let [red (rest (map-indexed
                   (fn [i a]
                     (assoc a :id i))
                   (reductions
                    (fn [{:keys [start-time end-time]} v]
                      (process-music end-time v))
                    {:start-time start-time
                     :end-time start-time}
                    durationables)))
        start    (:start-time (first red))
        duration (if (instance? LazySeq durationables)
                   :inf
                   (subtract-time (:end-time (last red)) start))]
    {:type :sequence
     :duration duration
     :start-time start-time
     :end-time   (add-time start-time duration)
     :children red}))

(defmethod process-music := [start-time [_ durationables]]
  (let [red
        (map-indexed
         (fn [i a]
           (assoc a :id i))
         (map #(process-music start-time %) durationables))
        duration (reduce max-time (map :duration red))]
    {:type :parallel
     :start-time start-time
     :end-time (add-time start-time duration)
     :duration duration
     :children red}))

(defmethod process-music :modify-state [start-time [_ {:keys [state-update children]}]]
  (let [red (process-music start-time children)]
    (assoc (select-keys red [:start-time :end-time :duration])
           :type :modify-state
           :state-update state-update
           :children red)))

#_(defcard "Processing the music for these notes in sequence"
  (process-music 0 [:+ [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]))

#_(defcard "Processing the music for these notes in sequence"
  (process-music 0 [:+ [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}
                        [:+ [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]]]))

#_(defcard "Processing the music for these notes in Parallel"
  (process-music 0 [:= [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}
                        [:= [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]]]))

#_(defcard "Processing the music for these notes in Parallel"
  (process-music 0 [:+
                    [[:= [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}
                          [:= [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]]]
                     [:= [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]]]))

#_(defcard "Processing infinite duration"
  (process-music 0 [:+
                    [[:+ (map identity [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}])]
                     [:+ [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]]]))

#_(defcard "Process modify state"
  (process-music 0 [:modify-state {:state-update :hey
                                   :children [:+ [{:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]]}]))

#_(defcard "Processing the music for these notes in parallel"
  (process-music 0 [:= {:pitch "E4" :duration 0.5} {:pitch "F4" :duration 0.5}]))

(defn durations-start-stop [durationables]
  (reductions (fn [{:keys [start end]} {:keys [duration] :as v}]
                {:start end
                 :end (+ end duration)
                 :value v})
              {:start 0 :end 0}
              durationables))

#_(defcard durations-start-stop
     (take 5 (durations-start-stop (repeat {:duration 1}))))

;; really just need this pattern of values being created once and
;; merged into the state that is being passed along
(defonce-react-class CurrentVal 
  #js
  {:componentWillMount
   (fn []
     (this-as
      this
      (.setState
       this
       #js {:data
            (into {}
                  (map (fn [[k vf]] [k (if (fn? vf) (vf) vf)])
                       (.. this -props -toMerge)))})))
   :componentWillUnmount
   (fn []
     (this-as this
              (when (.. this -props -unmountFn)
                ((.. this -props -unmountFn) this)
                )))
   :render
   (fn []
     (this-as
      this
      (let [current-state (merge (.. this -props -currentState)
                                 (.. this -state -data))]
        (sab/html
         [:div.cur-val
          (map
           (fn [c]
             (c current-state))
           (.. this -props -children))]))))})

(defn current-val
  [state key-vals children]
   (let [unmount-fn (:unmount-fn key-vals)]
     (js/React.createElement
      CurrentVal
      #js { :currentState state
            :toMerge      (dissoc key-vals :unmount-fn)
            :unmountFn    unmount-fn }
      children)))


(defn basic-main-synth []
  (prn "Creating synth")
  (let [synth 
        (.setPreset (js/Tone.PolySynth. 4 js/Tone.FMSynth) "Pianoetta")]
    (.toMaster synth)                                   
    synth))

(defn current-time* [state]
  (* (or (:speed state) 0.5)
     js/Tone.context.currentTime))

(defn play-at* [state time]
  (/  time (:speed state)))

(declare fm-synth)

(defn initial-player-state
  ([options]
   (let [speed (or (:speed options) default-play-speed)]
     (merge
      {:speed speed}
      options)))
  ([]
   (initial-player-state {})))

;; beginning api

;; this handling of rest is wrong
(defn note [n]
  (cond
    (= ::rest n) ::rest
    (and (or (string? n)
               (symbol? n)
               (keyword? n))
         (= "rest" (name n)))
    ::rest
    :else (p/note n)))

(defn handle-pitch [p]
  (if (coll? p)
    (map note p)
    (note p)))

(defn n*
  ([pitch duration]
   {:pitch pitch
    :duration duration})
  ([pitch duration vel]
   {:pitch pitch
    :duration duration :velocity vel})
  ([pitch duration vel sus]
   {:pitch pitch
    :duration duration :velocity vel :sustain sus}))

(defn n [& args]
  (apply n* (handle-pitch (first args)) (rest args)))

(def ch n)

(defn mod-state [state-update children]
  [:modify-state
   {:state-update state-update
    :children children}])

(defn use-synth [synth-fn children]
  
  (mod-state
   {:synth synth-fn
    :unmount-fn (fn [this]
                  (prn "Removing custome synth")
                  (when (fn? synth-fn)
                    (.dispose (:synth (.. this -state -data)))))}
   children))

(defn music-length [music]
  (reduce + 0 (map :duration music)))

(defn pad-to
  ([fin-music-seq total open]
   (let [length (music-length fin-music-seq)
         tail-rest (- total length open)
         open    (when-not (zero? open)
                   [(n "rest" open)])]
     (concat open
             fin-music-seq
             [(n "rest" tail-rest)])))
  ([fin-music-seq total]
   (pad-to fin-music-seq total 0)))

(defn mloop* [music]
  (mapcat identity (repeat music)))

(defn mloop [music]
  [:+ (mloop* music)])

(defn coerce-pitch [p]
  (cond
    (= p ::rest) ::rest
    (or (array? p) (coll? p))
    (to-array (map p/midi->hz
                   p))
    (integer? p) (p/midi->hz p)
    :else p))

(defn note-play [note state]
  (let [{:keys [pitch duration start-time sustain velocity]} note
        pitch (coerce-pitch pitch)
        play-time (+ (:start-time state) start-time)]
    (comment
      (prn    (:speed state))
      (prn    (:start-time state))    
      (prn                           (current-time* state))
      (prn                            pitch duration play-time)
      (prn (play-at* state play-time)))
    (when-not (:synth state)
      (.error js/console "No SYNTH defined"))
    (when (and (>= play-time (current-time* state))
               (not= pitch ::rest)
               (:synth state))
      (if (instance? js/Tone.NoiseSynth (:synth state))
        (.triggerAttackRelease (:synth state)
                               pitch
                               (play-at* state play-time)
                               (or velocity 1))
        (.triggerAttackRelease (:synth state)
                               pitch
                               (or sustain 0.3)
                               (play-at* state play-time)
                               (or velocity 1))))))

(defonce-react-class Noter
  #js
  {#_:shouldComponentUpdate
   #_(fn [next-props, next-state]
     (this-as
      this
      (or (not= (.-data next-props)     (.. this -props -data))
          (not= (.-playTime next-props) (.. this -props -playTime)))))
   ;; :componentDidUpdate (fn [] (this-as this (note-play this)))
   :componentDidMount (fn [] (this-as
                             this
                             (note-play
                              (.. this -props -data)
                              (.. this -props -currentState))))
   :render
   (fn []
     (this-as
      this
      (sab/html
       [:div.note {:key (str "note-" )}
        (if-let [renderer (:render-fn (.. this -props -data))]
          (renderer this)
          (prn-str
           (:pitch (.. this -props -data))))])))})

(defmulti render-music
  (fn [x]
    (if-let [type (:type x)]
      type
      (if (:pitch x) :note))))

(defmethod render-music :note [note state]
  (js/React.createElement
   Noter
   #js {:key (str (:pitch note) "-" (:duration note)  "-" (:id note))
        :currentState state
        :data note}))

(defmethod render-music :parallel [parallel state]
  (sab/html [:div.parallel
             (map
              #(render-music % state)
              (:children parallel))]))

(defn skip-to-playable [current-state music-sequence]
  (let [cur (current-time* current-state)]
    (drop-while (fn [{:keys [start-time end-time]}]
                  (>=  cur (+ end-time (:start-time current-state))))
                music-sequence)))

(defn create-skipper [lazy-seq]
  (let [state (atom lazy-seq)]
    (fn [current-state]
      (let [cur (skip-to-playable current-state @state)]
        (when (not= (hash (first cur)) (hash (first @state)))
          (reset! state cur))
        cur))))

;; need to work on getting this reloaded
(defonce-react-class PlaySequenceNew
  #js
  {:getInitialState
   (fn [] #js {})
   :componentWillMount
   (fn []
     (this-as
      this
      (when (.. this -props)
        (let [data (.. this -props -data)]
          (.setState this #js {:skipper (create-skipper (:children data))})))))
   :componentWillReceiveProps
   (fn [next-props]
     (this-as
      this
      
      (when-let [props  (.. this -props)]
        (let [rel   (.. props -reloadCount)
              n-rel (.. next-props -reloadCount)
              ch   (:children (.. props -data))
              n-ch (:children (.. next-props -data))]
          (when (or
                 (not= (hash (first ch))
                       (hash (first n-ch)))
                 (not= rel n-rel))
            (.setState this #js {:skipper (create-skipper n-ch)}))))))   
   :render
   (fn []
     (this-as
      this
      (let [data          (.. this -props -data)
            current-state (.. this -props -currentState)
            playables 
            (or ((.. this -state -skipper) current-state)
                nil)]
        (if (first playables)
          (sab/html [:div {:key "music"}
                     (map
                      #(render-music % current-state)
                      (take 4 playables))])
          #_(sab/html [:div "no music"])))))})


(defmethod render-music :sequence [sequence state]
  (js/React.createElement PlaySequenceNew #js {:currentState state
                                               :reloadCount @reload-atom
                                               :data sequence}))

(defmethod render-music :modify-state [{:keys [state-update children]} state]
  (current-val
   state
   state-update
   [(fn [s] (render-music children s) )]))

(defonce-react-class MusicRoot
  #js {:getInitialState
       (fn [] #js {:running false})
       :render
       (fn []
         (this-as
          this
          (let [state-atom (.. this -props -state_atom)
                music      (.. this -props -music)
                running    (.. this -state -running)]
            (if-not (:speed @state-atom)
              (do
                (prn @state-atom)
                (prn "Initializing State Atom")
                (reset! state-atom (initial-player-state))
                (sab/html [:div "No initial state"]))
              (do
                (when running
                  (js/setTimeout #(.forceUpdate this) 200))
                (sab/html
                 [:div.music-root
                  [:button {:onClick (fn [_] (.setState this #js {:running (not running)}))}
                   (if running "stop" "start")]
                  [:button {:onClick (fn [_]
                                       (.setState this #js {:running false})
                                       (swap! state-atom dissoc :start-time)
                                       (js/setTimeout #(.setState this #js {:running true}) 200))}
                   "restart"]
                  (when (and @state-atom running)
                    (let [state (if (:start-time @state-atom)
                                  @state-atom
                                  (let [st (+ (current-time* @state-atom) 0.5)]
                                    (js/setTimeout #(swap! state-atom assoc :start-time st) 0)
                                    (assoc @state-atom :start-time st)))]
                      (render-music
                       (process-music 0
                                      (use-synth 
                                       (or (:synth @state-atom) @default-synth)
                                       music))
                       state)))]))
              ))))})

;; also without react
(defmulti play-music
  (fn [x]
    (if-let [type (:type x)]
      type
      (if (:pitch x) :note))))

(defmethod play-music :note [note state]
  (note-play note state))

(defmethod play-music :parallel [parallel state]
  (mapv
   #(play-music % state)
   (:children parallel)))

(defmethod play-music :sequence [sequence state]
  (let [music-seq (create-skipper (:children sequence))]
    (letfn [(run-loop [last reload-count]
              (let [cur (first (music-seq
                                (update-in state
                                           [:start-time]
                                           #(- % 0.1))))]
                (when-not (or (nil? cur) (not= reload-count @reload-atom))
                  (when (not= cur last)
                    (play-music cur state))
                  (js/requestAnimationFrame #(run-loop cur reload-count)))))]
      (run-loop nil @reload-atom))))


#_(defmethod play-music :sequence [sequence state]
  (go
    (loop [music-seq (create-skipper (:children sequence))
           last nil
           reload-count @reload-atom]
      (let [cur (first (music-seq
                        (update-in state
                                   [:start-time]
                                   #(- % 0.1))))]
        (when-not (or (nil? cur) (not= reload-count @reload-atom))
          (when (not= cur last)
            (play-music cur state))
          (<! (timeout 20))
          (recur music-seq cur reload-count))))))

;; doesn't handle function params yet
(defmethod play-music :modify-state [{:keys [state-update children]} state]
  (play-music
       children
       (merge
        state
        state-update)))

(defn play-music! [state music]
  (let [modded (int
                (/ (-
                    (current-time* state)
                    (:start-time state)) 4))
        new-start
        (+ (:start-time state)
           (* modded 4))]
    (play-music (process-music 0 music)
                (assoc 
                 state
                 :start-time
                 new-start))))

(defcard play-music-cardd
  (fn [da o]
    #_(play-music! @da (mloop (map #(n % 0.25) [55 55 57 59]))))
  (initial-player-state
   { :start-time (+ (current-time* {:speed 0.41}) 0.1)
     :synth @default-synth}))

(defn music-root [state-atom music]
  (js/React.createElement
   MusicRoot #js {:music music :state_atom state-atom}))

(defn music-root-card [music]
  (fn [data-atom owner]
    (music-root data-atom music)))



#_(def apres-base-line-top
  (map (fn [x] (update-in
               x [:pitch] 
               (fn [p] (if (string? p)
                        "rest"
                        (aget p 0)))))
       apres-base-line))

#_(def apres-base-line-bottom
  (map (fn [x] (update-in
               x [:pitch] 
               (fn [p] (if (string? p)
                        p
                        (aget p 1)))))
       apres-base-line))

#_(defcard top apres-base-line-top)

(defn main-synth []
  (prn "Creating main-synth synth")
  (let [synth (js/Tone.PolySynth.
               4
               js/Tone.FMSynth
               #js { :volume 0.5
                    :carrier #js { :envelope #js {:release 0.8 }
                                  :filterEnvelope #js {:release 0.8 } }
                    :modulator #js { :envelope #js {:release 0.8 }
                                    :filterEnvelope #js {:release 0.8 } }
                    }) 
        #_(.setPreset "Pianoetta")]
    (.toMaster synth)                                   
    synth))

(defn single-fm-synth []
  (prn "Creating synth")
  (let [synth (js/Tone.FMSynth.
               #js {
                    :carrier #js { :envelope #js {:release 0.8 }
                                  :filterEnvelope #js {:release 0.8 } }
                    :modulator #js { :envelope #js {:release 0.8 }
                                    :filterEnvelope #js {:release 0.8 } }
                    }
               ) 
        #_(.setPreset "Pianoetta")]
    (.toMaster synth)                                   
    synth))



(defn main []
  ;; conditionally start the app based on wether the #main-app-area
  ;; node is on the page
  (if-let [node (.getElementById js/document "main-app-area")]
    (js/React.render (sab/html [:div "This is working"]) node)))

(main)

;; remember to run lein figwheel and then browse to
;; http://localhost:3449/devcards.html




