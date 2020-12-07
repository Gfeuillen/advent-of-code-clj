(ns advent-of-code.day5
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set]))



(defn parse-input [line]
  {:row (take 7 line) :col (take-last 3 line)})


(defn dichotomy [seq lv start end]
  (do
    (if (empty? seq)
    start
    (if (= (first seq) lv)
      (dichotomy (rest seq) lv (/ (+ end start) 2) end)
      (dichotomy (rest seq) lv start (/ (+ end start) 2))
      )
    ))
  )


(defn find-seat [ids]
  (if (= (- (second ids) (first ids)) 1)
    (find-seat (rest ids))
    (+ (first ids) 1)
    )
  )


(defn -main
  [& args]

  (def data-file
    (io/resource "inputday5.txt"))

  (def lines
    (line-seq (clojure.java.io/make-reader data-file [])))

  (->>
    (map parse-input lines)
    (map (fn [input]
           {:row (dichotomy (get input :row) \B 0 128)
            :col (dichotomy (get input :col) \R 0 8)}))
    (map (fn [seat] (+ (* (get seat :row) 8) (get seat :col))))
    sort
    (into [])
    find-seat
    println)
  )
