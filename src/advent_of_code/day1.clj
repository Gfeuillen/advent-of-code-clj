(ns advent-of-code.day1
  (:gen-class)
  (:require [clojure.java.io :as io]))

(def data-file
  (io/resource "inputday1.txt"))

(def numbers
  (map
    (fn [x] (Integer/parseInt x))
    (line-seq (clojure.java.io/make-reader data-file []))))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))


(def pairs
  (cart [numbers numbers]))

(def triples
  (cart [numbers numbers numbers]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println )
  (println
    (filter
    (fn [v] (= (get v 0) 2020))
    (map
      (fn [x] (vector (reduce + x) (reduce * x)))
      pairs)))                                              ; replace here to use the triples variable
  )
