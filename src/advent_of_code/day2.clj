(ns advent-of-code.day2
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn isvalid-old [l u c p]
  (def ccount (->>
                (seq p)
                (filter (fn [x] (= x c)))
                (count)
                ))
  (and (>= ccount l) (<= ccount u))
  )

(defn isvalid-new [pos1 pos2 c p]
  (def pos1-is-c (= c (get p (- pos1 1))))
  (def pos2-is-c (= c (get p (- pos2 1))))
  (and
    (or pos1-is-c pos2-is-c)
    (not (and pos1-is-c pos2-is-c))
    )
  )

(def data-file
  (io/resource "inputday2.txt"))

(defn parse-input [x]
  (def raw-input
    (->>
      (re-matcher #"(\d+)-(\d+) ([a-z]): ([a-z]*)" x)
      (re-find)
      (rest)
      (into [])
      ))
  (vector
    (Integer/parseInt (get raw-input 0))
    (Integer/parseInt (get raw-input 1))
    (.charAt (get raw-input 2) 0)
    (get raw-input 3))
  )


(def inputs
  (map
    parse-input
    (line-seq (clojure.java.io/make-reader data-file []))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;(println (isvalid 1 2 \a ""))
  ;(println (parse-input "6-9 b: bbbbbbbbb"))
  ;(def parsed-input (parse-input "6-9 b: bbbbbbbbb"))
  ;(println parsed-input)
  ;(println (apply isvalid parsed-input))

  ;(->>
  ;  (parse-input "2-9 c: ccccccccc")
  ;  (apply isvalid-new)
  ;  (println))

  (->>
    (map parse-input (line-seq (clojure.java.io/make-reader data-file [])))
    (filter (fn [x] (apply isvalid-new x)))
    (count)
    (println)
    )
  )
