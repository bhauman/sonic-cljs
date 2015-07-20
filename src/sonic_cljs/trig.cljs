(ns
    ^{:doc "Handy trig fns"
      :author "Sam Aaron"}
  sonic-cljs.trig
  (:require-macros
   [cljs.test :refer [is]]
   [devcards.core :refer [deftest]]))

(defn quantize
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

(defn cosr
  "Scaled, shifted (i.e. mul-add) cosine fn with the frequency specified in
  terms of the idx (typically representing the beat).

  Returns a value at idx along a scaled cosine fn with specified centre and
  range. The frequency is defined to be period idxs. Similar to Impromptu's
  cosr.

  (cosr 0 2 10 8) ;=> 12
  (cosr 2 2 10 8) ;=> 10
  (cosr 4 2 10 8) ;=> 8
  (cosr 6 2 10 8) ;=> 10
  (cosr 8 2 10 8) ;=> 12"
  [idx range centre period]
  (+ centre (* range (js/Math.cos (* 2 js/Math.PI idx (/ 1 period))))))


(deftest cosr-tests
  (is (= (cosr 0 2 10 8) 12))
  (is (= (cosr 2 2 10 8) 10)))

(defn sinr
  "Scaled, shifted (i.e. mul-add) sine fn with the frequency specified in
  terms of the idx (typically representing the beat).

  Returns a value at idx along a scaled sine fn with specified centre and
  range. The frequency is defined to be period idxs. Similar to Impromptu's
  sinr.

  (sinr 0 2 10 8) ;=> 10
  (sinr 2 2 10 8) ;=> 12
  (sinr 4 2 10 8) ;=> 10
  (sinr 6 2 10 8) ;=> 8
  (sinr 8 2 10 8) ;=> 10"
  [idx range centre period]
  (+ centre (* range (js/Math.sin (* 2 js/Math.PI idx (/ 1 period))))))

(defn tanr
  "Scaled, shifted (i.e. mul-add) tan fn with the frequency specified in
  terms of the idx (typically representing the beat).

  Returns a value at idx along a scaled tan fn with specified centre and
  range. The frequency is defined to be period idxs. Similar to Impromptu's
  tanr.

  (tanr 0 2 10 8)        ;=> 10
  (tanr 1.999999 2 10 8) ;=> ~2546489
  (tanr 2 2 10 8)        ;=> ~3.2665
  (tanr 4 2 10 8)        ;=> 10
  (tanr 5.999999 2 10 8) ;=> ~2546489
  (tanr 6 2 10 8)        ;=> ~1.0886
  (tanr 8 2 10 8)        ;=> 10"
  [idx range centre period]
  (+ centre (* range (js/Math.tan (* 2 js/Math.PI idx (/ 1 period))))))
