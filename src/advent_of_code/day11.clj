(ns advent-of-code.day11
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn parse-input [lines]
  (apply merge
         (apply concat
                (map-indexed
                  (fn [j line]
                    (map-indexed
                      (fn [i e] {(list i j) (case e
                                              \L 0
                                              \. nil
                                              \# 1)}) line)) lines)))
  )


(defn simple-neighbourhood [places [x y]]
  (apply +
         (conj
           (filter
             (fn [x] (not (nil? x)))
             (list
               (get places (list x (inc y)) 0)
               (get places (list (inc x) (inc y)) 0)
               (get places (list (inc x) y) 0)
               (get places (list (inc x) (dec y)) 0)
               (get places (list x (dec y)) 0)
               (get places (list (dec x) (dec y)) 0)
               (get places (list (dec x) y) 0)
               (get places (list (dec x) (inc y)) 0)))
           0)
         )
  )


(defn is-occupied [places sx sy dx dy]
  (let [next-x (+ sx dx)
        next-y (+ sy dy)
        next-k (list next-x next-y)]
    (if (contains? places next-k)
      (if (nil? (get places next-k))
        (is-occupied places next-x next-y dx dy)
        (get places next-k))
      0)
    )
  )


(defn large-neighbourhood [places [x y]]
  (apply +
         (conj
           (filter
             (fn [x] (not (nil? x)))
             (list
               (is-occupied places x y 0 1)
               (is-occupied places x y 1 0)
               (is-occupied places x y 1 1)
               (is-occupied places x y 0 -1)
               (is-occupied places x y -1 0)
               (is-occupied places x y -1 -1)
               (is-occupied places x y -1 1)
               (is-occupied places x y 1 -1)))
           0)
         )
  )

(defn fixed-point [places changed nbhf cut]
  (if (not changed)
    places
    (let [up-places (map
                      (fn [[k v]]
                        (let [new-v
                              (if (nil? v)
                                v
                                (let [nbh (nbhf places k)]
                                  (if (and (= v 1) (>= nbh cut))
                                    0
                                    (if (and (= 0 nbh) (= 0 v))
                                      1
                                      v)
                                    ))
                                )]
                          [{k new-v} (not (= v new-v))]
                          )
                        )
                      places)
          new-places (apply merge (map (fn [x] (get x 0)) up-places))
          p-changed (filter (fn [[x c]] c) up-places)
          changed (> (count p-changed) 0)]
      (fixed-point new-places changed nbhf cut)
      )
    )
  )

(defn -main
  [& args]

  (def data-file
    (io/resource "inputday11.txt"))
  (def lines (str/split (slurp data-file) #"\n"))

  (let [input (parse-input lines)
        part-1 (fixed-point input true simple-neighbourhood 4)
        part-2 (fixed-point input true large-neighbourhood 5)
        ]

    (println
      (count
        (filter
          (fn [[k v]] (and (not (nil? v)) (= v 1)))
          part-1)))

    (println
      (count
        (filter
          (fn [[k v]] (and (not (nil? v)) (= v 1)))
          part-2)))
    )
  )
