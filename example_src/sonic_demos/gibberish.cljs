(ns sonic-demos.gibberish)

(defn l [arg]
  (.log js/console arg))

#_(defonce gibberish (js/Gibberish.init))

#_(defn fm-synth []
  (let [fm (js/Gibberish.FMSynth. #js {:cmRatio 3
                                       :index 10.2
                                       :attack 1050
                                       :decay  5050
                                       :sustain 2500                                       
                                       :release 21025
                                       :useADSR true
                                       })]
    (doto fm
      
      (.connect))))





#_(def fm (fm-synth))


#_(.note fm 140 0.5)
