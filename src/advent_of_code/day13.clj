(ns advent-of-code.day13
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn read-lines [file-name]
  (let [res (io/resource file-name)]
    (str/split (slurp res) #"\n")
    )
  )

(defn parse-lines [lines]
  (let
    [depart-time (Integer/parseInt (first lines))
     buses (filter (fn [[i b]] (not (= "x" b))) (map-indexed (fn [i, b] [i, b]) (str/split (second lines) #",")))]
    {:depart depart-time
     :buses  (map (fn [[i b]] [i (Integer/parseInt b)]) buses)}
    )
  )

(defn sol1 [input]
  (let [depart (:depart input)
        earliests (map
                    (fn [b] {:d (+ (- depart (mod depart b)) b) :id b})
                    (map #(get % 1) (:buses input)))
        earliest (apply min-key :d earliests)]
    (* (- (:d earliest) depart) (:id earliest))
    )
  )

(defn sol2 [input]
  (loop [step 1
         start-time 0
         buses (:buses input)]
    (if (empty? buses)
      start-time
      (let [[idx bus-id] (first buses)]
        (if (= (mod (+ start-time idx) bus-id) 0)
          (recur (* step bus-id) start-time (rest buses))
          (recur step (+ start-time step) buses)
          )
        ))
    )

  )

(defn -main
  [& args]
  (let [parse-input (comp parse-lines read-lines)
        sol-1 (comp sol1 parse-input)
        sol-2 (comp sol2 parse-input)]
    (println (sol-1 "inputday13-test.txt"))
    (println (sol-2 "inputday13-test.txt"))
    (println (sol-1 "inputday13-test2.txt"))
    (println (sol-2 "inputday13-test2.txt"))
    (println (sol-1 "inputday13.txt"))
    (println (sol-2 "inputday13.txt"))
    )
  )
