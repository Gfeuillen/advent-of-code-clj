(ns advent-of-code.day23
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]))

(defn parse-input [line]
  (into [] (map #(Integer/parseInt (str %)) line)))

(defn in?
  [coll elm]
  (some #(= elm %) coll))

(defn take-n [n state current]
  (loop [acc (list)
         c (get state current)]
    (if (= (count acc) n)
      (reverse acc)
      (recur (conj acc c) (get state c)))))

(def take-3 (partial take-n 3))

(defn play-crabs-2 [n rounds input]
  (let [extra-elements (range (inc (apply max input)) (inc n))
        mapped-input (zipmap (concat input extra-elements)
                             (concat (rest input) extra-elements [(first input)]))]
    (loop [state mapped-input
           current (first input)
           played-rounds 0
           max-val (apply max (vals mapped-input))]
      ;(if (= (mod played-rounds 1000000) 0)  (println played-rounds)) ;print progress
      (let [taken (take-3 state current)]
        (if (= played-rounds rounds)
          state
          (let [potential-target (range (dec current) 0 -1)
                dec-target-in-game (first (filter #(not (in? taken %)) potential-target))
                potential-from-max-val (range max-val 0 -1)
                from-max-val (first (filter #(not (in? taken %)) potential-from-max-val))
                target (or dec-target-in-game from-max-val)
                new-state (assoc state
                            target (first taken)
                            (last taken) (get state target)
                            current (get state (last taken)))]
            (recur new-state (get new-state current) (inc played-rounds) max-val)))))))

(defn compute-sol-1 [state]
  (str/join (take-n (dec (count state)) state 1)))

(defn compute-sol-2 [state]
  (apply * (take-n 2 state 1)))

(defn -main
  [& args]
  (let [sol1 (comp compute-sol-1 (partial play-crabs-2 0 100) parse-input)
        sol2 (comp compute-sol-2 (partial play-crabs-2 1000000 10000000) parse-input)]
    (pp/pprint (sol1 "389125467"))
    (pp/pprint (sol1 "871369452"))
    (pp/pprint (sol2 "389125467"))
    (pp/pprint (sol2 "871369452"))))
