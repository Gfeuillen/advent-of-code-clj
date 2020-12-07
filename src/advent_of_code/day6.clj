(ns advent-of-code.day6
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set]))

(defn -main
  [& args]

  (def data-file
    (io/resource "inputday6.txt"))

  (->>
    (str/split (slurp data-file) #"\n\n")
    (map (fn [group] (->>
                       (str/split group #"\n")
                       (map set)
                       (reduce clojure.set/intersection)    ; Use (reduce clojure.set/union) for the first question
                       count)))
    (reduce +)
    println
    )

  )
