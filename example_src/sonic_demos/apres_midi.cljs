(ns sonic-demos.apres-midi
  (:require
   [devcards.core :as dc]
   [sonic-cljs.core :as sc :refer [n ch initial-player-state mloop music-root-card use-synth]]
   [sonic-cljs.webaudio :as wa]
   [sablono.core :as sab :include-macros true])
  (:require-macros
   [devcards.core :refer [defcard deftest]]))


(def apres-intro
  (apply
   concat
   (take
    2
    (repeat
     (map
      (fn [x] (assoc x :sustain 0.3))
      [(n "rest" 0.125)
       (n "G4" 0.0625 1 1)
       (n "F#4" 0.0625 0.5 1)
       (n "G4" 0.125 0.8 1)
       (n "B4" 0.0625 1 1)
       (n "C5" 0.0625 0.4 1)
       (n "B4" 0.5 0.8 1)
       
       (n "rest" 0.125)
       (n "F#4" 0.0625 1 1)
       (n "G4" 0.0625 1 1)
       (n "F#4" 0.125 1 1)
       (n "G4" 0.0625 1 1)
       (n "A4" 0.0625 1 1)
       (n "G4" 0.5 1 1)
       
       (n "rest" 0.125)
       (n "F#4" 0.0625 1 1)
       (n "E4" 0.0625 1 1)
       (n "F#4" 0.125 1 1)
       (n "B4" 0.0625 1 1)
       (n "C5" 0.0625 1 1)
       (n "B4" 0.5 1 1)
       
       (n "rest" 0.125)
       (n "F#4" 0.0625 1 1)
       (n "E4" 0.0625 1 1)
       (n "F#4" 0.75 1 0.5)])))))


(defn map-pitch* [f pitch]
  (cond
    (= pitch ::sc/rest) pitch
    (coll? pitch) (map f pitch)
    :else (f pitch)))

(defn map-pitch [f note]
  (update-in note [:pitch] (fn [p] (map-pitch* f p))))

(defn pitch-adjust [adj]
  (fn [n]
    (map-pitch #(+ adj %) n)))


#_(prn (take 5 (map (pitch-adjust -12) apres-intro)))

(prn       (n "G5" 0.0625 1 1.5))

(def apres-intro2
  (apply
   concat
   (take
    2
    (repeat
     [(n "rest" 0.125)
      (n "G5" 0.0625 1 1.5)
      (n "F#5" 0.0625 1 1.5)
      (n "G5" 0.125 1 1.5)
      (n "B5" 0.0625 1 1.5)
      (n "C6" 0.0625 1 1.5)
      (n "B5" 0.5 1 0.8)
      
      (n "rest" 0.125)
      (n "F#5" 0.0625 1 1.5)
      (n "G5" 0.0625 1 1.5)
      (n "F#5" 0.125 1 1.5)
      (n "G5" 0.0625 1 1.5)
      (n "A5" 0.0625 1 1.5)
      (n "G5" 0.5 1 0.8)
      
      (n "rest" 0.125)
      (n "F#5" 0.0625 1 1.5)
      (n "E5" 0.0625 1 1.5)
      (n "F#5" 0.125 1 1.5)
      (n "B5" 0.0625 1 1.5)
      (n "C6" 0.0625 1 1.5)
      (n "B5" 0.5 1 0.8)
      
      (n "rest" 0.125)
      (n "F#5" 0.0625 1 1.5)
      (n "E5" 0.0625 1 1.5)
      (n "F#5" 0.75 1 0.5)]))      ))

(def apres-stacatto
  [(n "E5" 0.375 1 0.4)
   (n "B4" 0.625 1 0.4)

   (n "D5" 0.375 1 0.4)
   (n "B4" 0.625 1 0.4)

   (n "F#5" 0.375 1 0.4)
   (n "B4" 0.625 1 0.4)

   (n "F#5" 0.375 1 0.4)
   (n "A4" 0.625 1 0.4)   

   (ch ["B4" "G5"] 0.375 1 0.4)
   (ch ["G4" "E5"] 0.625 1 0.4)      

   (ch ["B4" "G5"] 0.375 1 0.4)
   (ch ["G4" "D5"] 0.625 1 0.4)

   (ch ["B4" "F#5"] 0.375 1 0.4)
   (ch ["F#4" "D5"] 0.625 1 0.4)

   (ch ["A4" "F#5"] 0.375 1 0.4)
   (ch ["F#4" "D5"] 0.625 1 0.4)            

   ])

(def apres-stacatto2
  [(n "E6" 0.375 1 0.4)
   (n "B5" 0.625 1 0.4)

   (n "D6" 0.375 1 0.4)
   (n "B5" 0.625 1 0.4)

   (n "F#6" 0.375 1 0.4)
   (n "B5" 0.625 1 0.4)

   (n "F#6" 0.375 1 0.4)
   (n "A5" 0.625 1 0.4)   

   (ch ["B5" "G6"] 0.375 1 0.4)
   (ch ["G5" "E6"] 0.625 1 0.4)      

   (ch ["B5" "G6"] 0.375 1 0.4)
   (ch ["G5" "D6"] 0.625 1 0.4)

   (ch ["B5" "F#6"] 0.375 1 0.4)
   (ch ["F#5" "D6"] 0.625 1 0.4)

   (ch ["A5" "F#6"] 0.375 1 0.4)
   (ch ["F#5" "D6"] 0.625 1 0.4)            

   ])

(defn arpeg-part [begin end]
  (concat (apply concat
                 (take 4
                       (repeat begin)))
          end))

(def apres-arpeggio
  (concat
     (map #(n % 0.0625 0.8)
       (concat
        (arpeg-part ["B4" "E5" "B5"] ["B4" "E5" "C5" "E5"])
        (arpeg-part ["B4" "D5" "B5"] ["B4" "D5" "A4" "D5"])
        (arpeg-part ["F#4" "B4" "F#5"] ["F#4" "B4" "G4" "B4"])
        (arpeg-part ["A4" "D5" "A5"] ["A4" "D5" "G4" "D5"])

        (arpeg-part ["B4" "E5" "B5"] ["B4" "E5" "C5" "E5"])
        (arpeg-part ["B4" "D5" "B5"] ["B4" "D5" "A4" "D5"])
        (arpeg-part ["F#4" "B4" "F#5"] ["F#4" "B4" "G4" "B4"])

        (concat (apply concat
                       (take 4
                             (repeat ["A4" "D5" "A5"])))
                ["A4" "D5"])))
     [(n "A5" 0.125 0.8)]))

(def apres-base-line
  (map
   (fn [x]
     (-> x
       (update-in [:velocity] #(/ % 1.5))
       ))
   [(ch ["E4" "E3"]
        0.125 0.9)
    (n "B3" 0.125 0.7)
    (ch ["E4" "G3"] 0.125 0.5)
    (n "B3" 0.125 0.7)
    
    (ch ["E4" "E3"] 0.125 0.9)
    (n "B3" 0.125 0.7)
    (ch ["E4" "G3"] 0.125 0.5)
    (n "B3" 0.125 0.7)
    
    (ch ["D4" "D2"] 0.125 0.9)
    (n "B3" 0.125 0.7)
    (ch ["D4" "G3"] 0.125 0.5)
    (n "B3" 0.125 0.7)
    
    (ch ["D4" "D2"] 0.125 0.9)
    (n "B3" 0.125 0.7)
    (ch ["D4" "G3"] 0.125 0.5)
    (n "B3" 0.125 0.7)
    
    
   (ch ["D4" "D3"] 0.125 0.9)
   (n "B3" 0.125 0.7)
   (ch ["D4" "F#3"] 0.125 0.5)
   (n "B3" 0.125 0.7)
   
   (ch ["D4" "D3"] 0.125 0.9)
   (n "B3" 0.125 0.7)
   (ch ["D4" "F#3"] 0.125 0.5)
   (n "B3" 0.125 0.7)
   
   
   (ch ["D4" "D3"] 0.125 0.9)
   (n "A3" 0.125 0.7)
   (ch ["D4" "F#3"] 0.125 0.5)
   (n "A3" 0.125 0.7)
   
   (ch ["D4" "D3"] 0.125 0.9)
   (n "A3" 0.125 0.7)
   (ch ["D4" "F#3"] 0.125 0.5)
   (n "A3" 0.125 0.7)]
   ))

(defonce music-state (atom (initial-player-state {:speed 0.41})))

(defcard music-state music-state)

(defcard apres-introo
  (music-root-card
   (use-synth (wa/piano wa/ivy-audio-piano) #_(wa/piano wa/steinway-grand)
              (mloop (concat
                      #_[(n 'rest 4)]
                      apres-intro
                      apres-stacatto
                      apres-arpeggio
                      (map (pitch-adjust +12) apres-intro)
                      (map (pitch-adjust +12) apres-stacatto)
                      apres-arpeggio))
             
              #_(mloop apres-base-line)))
  music-state)

(defcard apres-stacatto
  (music-root-card
   (mloop apres-stacatto))
  music-state)

(defcard apres-arpeggio
  (music-root-card
   (mloop apres-arpeggio))
  music-state)

(defcard apres-base-linee
  (music-root-card
   (use-synth (wa/piano wa/ivy-audio-piano)
              (mloop apres-base-line)))
  music-state)





