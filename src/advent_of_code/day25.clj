(ns advent-of-code.day25
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]))


(defn handshake [value subject]
  (let [step-1 (* value subject)
        step-2 (mod step-1 20201227)]
    step-2))

(defn loop-handshake [n subject]
  (loop [i 0
         value 1]
    (if (= i n)
      value
      (recur (inc i) (handshake value subject)))))

(defn find-loop-size [subject key]
  (loop [value 1
         loop 0]
    (if (= value key)
      loop
      (recur (handshake value subject) (inc loop)))))

(defn solve-1 [c-key d-key]
  (let [c-loop (find-loop-size 7 c-key)
        d-loop (find-loop-size 7 d-key)
        c-enc-key (loop-handshake c-loop d-key)
        d-enc-key (loop-handshake d-loop c-key)]
    [c-enc-key d-enc-key]
    )
  )

(defn -main
  [& args]
  (let []
    (pp/pprint (solve-1 12090988 240583))
    ))
