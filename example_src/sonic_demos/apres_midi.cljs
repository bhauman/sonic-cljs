(ns sonic-demos.apres-midi
  (:require
   [devcards.core :as dc]
   [sonic-cljs.core :as sc :refer [n ch initial-player-state mloop music-root-card use-synth main-synth]]
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
      (fn [x] (assoc x :sustain 0.1))
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

(def apres-intro2
  (apply
   concat
   (take
    2
    (repeat
     [(n "rest" 0.125)
      (n "G5" 0.0625 1 0.2)
      (n "F#5" 0.0625 1 0.2)
      (n "G5" 0.125 1 0.2)
      (n "B5" 0.0625 1 0.2)
      (n "C6" 0.0625 1 0.2)
      (n "B5" 0.5 1 0.2)
      
      (n "rest" 0.125)
      (n "F#5" 0.0625 1 0.2)
      (n "G5" 0.0625 1 0.2)
      (n "F#5" 0.125 1 0.2)
      (n "G5" 0.0625 1 0.2)
      (n "A5" 0.0625 1 0.2)
      (n "G5" 0.5 1 0.2)
      
      (n "rest" 0.125)
      (n "F#5" 0.0625 1 0.2)
      (n "E5" 0.0625 1 0.2)
      (n "F#5" 0.125 1 0.2)
      (n "B5" 0.0625 1 0.2)
      (n "C6" 0.0625 1 0.2)
      (n "B5" 0.5 1 0.2)
      
      (n "rest" 0.125)
      (n "F#5" 0.0625 1 0.2)
      (n "E5" 0.0625 1 0.2)
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
       (update-in [:velocity] #(/ % 2))
       ))
   [(ch ["E4" "E3"]
        0.135 1)
   (n "B3" 0.115 0.2)
   (ch ["E4" "G3"] 0.135 0.5)
   (n "B3" 0.115 0.2)

   (ch ["E4" "E3"] 0.135 1)
   (n "B3" 0.115 0.2)
   (ch ["E4" "G3"] 0.135 0.5)
   (n "B3" 0.115 0.2)
   
   (ch ["D4" "D3"] 0.125 1)
   (n "B3" 0.125 0.2)
   (ch ["D4" "G3"] 0.125 0.8)
   (n "B3" 0.125 0.2)
   
   (ch ["D4" "D3"] 0.125 1)
   (n "B3" 0.125 0.2)
   (ch ["D4" "G3"] 0.125 0.8)
   (n "B3" 0.125 0.2)
   
   
   (ch ["D4" "D3"] 0.125 1)
   (n "B3" 0.125 0.2)
   (ch ["D4" "F#3"] 0.125 0.8)
   (n "B3" 0.125 0.2)
   
   (ch ["D4" "D3"] 0.125 1)
   (n "B3" 0.125 0.2)
   (ch ["D4" "F#3"] 0.125 0.8)
   (n "B3" 0.125 0.2)
   
   
   (ch ["D4" "D3"] 0.125 1)
   (n "A3" 0.125 0.2)
   (ch ["D4" "F#3"] 0.125 0.8)
   (n "A3" 0.125 0.2)
   
   (ch ["D4" "D3"] 0.125 01)
   (n "A3" 0.125 0.2)
   (ch ["D4" "F#3"] 0.125 0.8)
   (n "A3" 0.125 0.2)]
   ))

(defonce music-state (atom (initial-player-state {:speed 0.41})))

(defcard music-state music-state)

(defcard apres-intro
  (music-root-card
   (mloop (concat
           #_[(n 'rest 4)]
           apres-intro
           apres-stacatto
           apres-arpeggio)))
  music-state)

(defcard apres-stacatto
  (music-root-card
   (mloop apres-stacatto))
  music-state)

(defcard apres-arpeggio
  (music-root-card
   (mloop apres-arpeggio))
  music-state)

(defcard apres-base-line
  (music-root-card
   (use-synth main-synth
              (mloop apres-base-line)))
  music-state)



