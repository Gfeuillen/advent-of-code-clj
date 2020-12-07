(ns advent-of-code.day7
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn parse-input [line]
  (def input-first-match (re-matches #"^([a-z]+ [a-z]+) bags contain (.*)$" line))
  {
   (get input-first-match 1)
   (map
     (fn [m] {:color (get m 3) :num (Integer/parseInt (get m 2))})
     (re-seq #"(\s?(\d+) ([a-z]+ [a-z]+) bags?[\,|\.])" (get input-first-match 2)))}
  )

(defn find-all-parent [k c-p-map]
  (if (contains? c-p-map k)
    (concat
      (get c-p-map k)
      (reduce concat (map (fn [p] (find-all-parent p c-p-map)) (get c-p-map k))))
    (list k)
    ))

(defn find-all-parent [k c-p-map]
  (if (contains? c-p-map k)
    (concat
      (get c-p-map k)
      (reduce concat (map (fn [p] (find-all-parent p c-p-map)) (get c-p-map k))))
    (list k)
    ))


(defn count-child [k p-c-map]
  (if (empty? (get p-c-map k))
    1
    (reduce
      +
      1
      (map
        (fn [e] (* (get e :num) (count-child (get e :color) p-c-map)))
        (get p-c-map k)))
    )
  )

(defn -main
  [& args]

  (def data-file
    (io/resource "inputday7.txt"))

  (def parent-child
    (->>
      (str/split (slurp data-file) #"\n")
      (map parse-input)
      (reduce conj)
      ))

  (println parent-child)

  (def child-parent
    (->>
      (map (fn [[k vs]] {k (map (fn [m] (get m :color)) vs)}) parent-child)
      (reduce conj)
      (map (fn [[k vs]] (map (fn [v] {v (list k)}) vs)))
      (reduce concat)
      (apply merge-with into)
      ))

  ; Solution to part 1
  (->>
    (find-all-parent "shiny gold" child-parent)
    set
    count
    println)

  ; Solution to part 2
  (println (- (count-child "shiny gold" parent-child) 1))
  )
