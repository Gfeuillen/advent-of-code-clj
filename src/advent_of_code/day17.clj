(ns advent-of-code.day17
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]))

(defn read-lines [file-name]
  (let [res (io/resource file-name)]
    (str/split (slurp res) #"\n")))

(defn parse-input [dim lines]
  (let [points (apply concat (map-indexed (fn [y line] (map-indexed (fn [x e] (if (= e \#) (concat (list x y) (repeat (- dim 2) 0)) nil)) line)) lines))]
    (into #{} (filter identity points))))

(defn neighbourhood [dim point]
  (let [changes (filter #(not (= % (repeat dim 0))) (combo/selections [1 0 -1] dim))]
    (into #{} (map #(map + point %) changes))))

(defn keep-active? [dim state cube]
  (let [neighbours (neighbourhood dim cube)
        active-neighbours (count (set/intersection state neighbours))]
    (<= 2 active-neighbours 3)))

(defn become-active? [dim state cube]
  (let [neighbours (neighbourhood dim cube)
        active-neighbours (count (set/intersection state neighbours))]
    (= active-neighbours 3)))

(defn solve [dim n input]
  (loop [state input
         i 0]
    (if (= i n)
      (count state)
      (let [kept-active (filter (partial keep-active? dim state) state)
            all-neighbours (reduce #(set/union %1 %2) (map (partial neighbourhood dim) state))
            inactive-neighbours (set/difference all-neighbours state)
            became-active (filter (partial become-active? dim state) inactive-neighbours)]
        (recur (into #{} (set/union kept-active became-active)) (inc i))))))

(defn -main
  [& args]
  (let [s1 (comp (partial solve 3 6) (partial parse-input 3) read-lines)
        s2 (comp (partial solve 4 6) (partial parse-input 4) read-lines)]
    ;(pp/pprint (s1 "inputday17-test.txt"))
    (pp/pprint (s1 "inputday17.txt"))
    ;(pp/pprint (s2 "inputday17-test.txt"))
    (pp/pprint (s2 "inputday17.txt"))))
