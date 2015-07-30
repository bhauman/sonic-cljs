(ns sonic-cljs.visual
  (:require
   [devcards.core :as dc]
   [sonic-cljs.webaudio :as wa]
   [sablono.core :as sab :include-macros true]) 
  (:require-macros
   [cljs-react-reload.core :refer [defonce-react-class def-react-class]]
   [devcards.core :refer [defcard deftest]]))

#_(defonce main-synth* (wa/fm-synth {:amp 0.1}))

#_(wa/-play-note main-synth* {:pitch 44 :velocity 1})

(defn ref->node [this ref]
  (when-let [comp (aget (.. this -refs) ref)]
    (js/React.findDOMNode comp)))

(defn get-frequency-value [freq freq-domain]
  (let [nyquist (/ (.-sampleRate wa/*context*) 2)
        index   (js/Math.round (*
                                (/ freq nyquist)
                                (.-length freq-domain)))]
    (aget freq-domain index)))

(defn draw-bar-graph [analyzer canvas-context canvas-node freq-domain]
  (let [bin-count      (.-frequencyBinCount analyzer)
        cWIDTH         (.-width canvas-node)
        cHEIGHT        (.-height canvas-node)]
    (.getByteFrequencyData analyzer freq-domain)

    (dotimes [index bin-count]
      (let [value (aget freq-domain index)
            percent (/ value 256)
            height  (* cHEIGHT percent)
            offset  (- cHEIGHT height)
            bar-width (/ cWIDTH bin-count)
            hue       (* 360 (/ index bin-count))]
        (set! (.-fillStyle canvas-context) (str "hsl(" hue ", 100%, 50%)"))
        (.fillRect canvas-context
                   (* index bar-width)
                   offset
                   bar-width
                   height)))))

(defn draw-line-graph [analyzer canvas-context canvas-node time-domain]
  (let [bin-count      (.-frequencyBinCount analyzer)
        cWIDTH         (.-width canvas-node)
        cHEIGHT        (.-height canvas-node)]
    (.getByteTimeDomainData analyzer time-domain)
    (set! (.-lineWidth canvas-context) 1)
    (set! (.-strokeStyle canvas-context) "rgba(255,255,255,1)")
    (.beginPath canvas-context)
    (dotimes [index bin-count]
      (let [value (aget time-domain index)
            percent (/ value 256)
            height  (* cHEIGHT percent)
            offset  (- cHEIGHT height)
            bar-width (/ cWIDTH bin-count)]
        
        (if (zero? index)
          (.moveTo canvas-context 0
                   offset)
          (.lineTo canvas-context
                   (* index bar-width)
                   offset)
          )))
    (.stroke canvas-context)))

(defonce-react-class Analyzer
  #js {:getInitialState
       (fn [] #js {:loop true})
       :componentDidMount
       (fn []
         (this-as
          this
          (when-not (.. this -state -analyzer)
            (set! (.. this -state -analyzer)
                  (let [analyzer (.createAnalyser wa/*context*)]
                    (.connect (or (.. this -props -listen_node)
                                  wa/*output*)
                              analyzer)
                    (set! (.. analyzer -fftSize) 128)
                    analyzer))
            (let [analyzer     (.. this -state -analyzer)
                  canvas-node (ref->node this "canvas") 
                  canvas-ctx  (.getContext (ref->node this "canvas") "2d")
                  freq-domain    (js/Uint8Array. (.-frequencyBinCount analyzer))
                  time-domain    (js/Uint8Array. (.-frequencyBinCount analyzer))
                  last-time (atom (js/Date.now))]
              (letfn [(render-loop [t]
                        (when (.. this -state -loop)
                          (js/requestAnimationFrame render-loop))
                        ;; this needs to be tighter and faster
                        ;; consider straight js
                        (when (> (- (js/Date.now) @last-time) 50)
                          (reset! last-time (js/Date.now))
                          (.clearRect canvas-ctx 0 0 (.-width canvas-node) (.-height canvas-node))
                          (set! (.-fillStyle canvas-ctx) "black")
                          (.fillRect canvas-ctx 0 0 (.-width canvas-node) (.-height canvas-node))
                          (draw-bar-graph analyzer canvas-ctx canvas-node freq-domain)
                          (draw-line-graph analyzer canvas-ctx canvas-node time-domain)
                          ))]
                (render-loop 0))))))

       :componentWillUnmount
       (fn []
         (this-as this
                  (let [ana (.. this -state -analyzer)]
                    (set! (.. this -state -loop) false)
                    (.disconnect ana))))
       :render (fn []
                 (this-as
                  this
                  (sab/html [:canvas {:ref "canvas"
                                      :width  (.. this -props -width)
                                      :height (.. this -props -height)}])
                  ))})

(defn analyzer
  ([{:keys [width height listen-node]
     :or {width 600
          height 150
          listen-node wa/*output*}}]
   (js/React.createElement Analyzer
                           #js {:width width
                                :height height
                                :listen-node listen-node}))
  ([] (analyzer {})))

#_(defcard analyzer-noder
  (analyzer {}))

(defn slider* [this {:keys [label value min max precision default-value
                      on-change]
               :or   {min 0
                      max 100
                      default-value 0
                      on-change identity}}]
  (let [precision (if (and (nil? precision) (= max 1))
                    6
                    (or precision 0))
        prec (js/Math.pow 10 precision)]
    (sab/html
     [:div
      [:label {:style { :paddingRight "10px"
                        :width "150px"
                       :display "inline-block"
                       :line-height "1.5em"
                       :fontSize    "1.2em"
                       :color "#888"}}
       label]
      [:input {:style { :width "400px"
                        :display "inline-block"
                        :marginRight "20px"}

               :type "range"
               :min (* min prec)
               :max (* max prec) 
               :defaultValue (* default-value prec)
               :onChange 
               (fn [e]
                 (let [v (/ (int (.. e -target -value))prec)]
                   (.setState this #js {:value v})
                   (on-change v)))}]
      [:span {:style { :width "70px"
                       :display "inline-block"
                       :marginRight "20px"}}
       (or (.. this -state -value) default-value)]
      ])))

(defonce-react-class Slider
  #js {:getInitialState
       (fn [] #js {})
       :render
       (fn []
         (this-as
          this
          (let [options (.. this -props -options)]
            (slider* this options))))})

(defn slider [options]
  (js/React.createElement Slider #js {:options options}))

(defcard slider-controler
  (slider {:label "Volumer"
           :min 0.0
           :max 10.0
           ; :precision 3
           :on-change (fn [v] (prn v))}))

(defn create-control [object schema]
  (sab/html
   [:div.option
    (slider
     (assoc schema
            :label    (:name schema)
            :on-change (fn [v]
                         ((:set! schema) v))))]))

(defn option-schema-controls [i-options-schema]
  (sab/html
   [:div.controls
    (map
     #(create-control i-options-schema %)
     (wa/-options-schema i-options-schema))]))

#_(defcard options-controlsss
  (option-schema-controls main-synth*))
