(ns advent-of-code.day24
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

(defn multiple-update [m updates]
  (r/fold (constantly m) (fn [acc [k f]]
                           (update acc (keyword k) f)) updates))

(defn reduce-tile [tile-instructions]
  (loop [state {:x 0
                :y 0}
         instructions tile-instructions]
    (if (empty? instructions)
      state
      (case (first instructions)
        \e (recur (update state :x inc) (rest instructions))
        \w (recur (update state :x dec) (rest instructions))
        \s (case (second instructions)
             \e (recur (multiple-update state
                                        [["y" dec]
                                         ["x" #(+ % 0.5)]]) (drop 2 instructions))
             \w (recur (multiple-update state
                                        [["y" dec]
                                         ["x" #(- % 0.5)]]) (drop 2 instructions))
             )
        \n (case (second instructions)
             \e (recur (multiple-update state
                                        [["y" inc]
                                         ["x" #(+ % 0.5)]]) (drop 2 instructions))
             \w (recur (multiple-update state
                                        [["y" inc]
                                         ["x" #(- % 0.5)]]) (drop 2 instructions)))))))

(defn solve-1 [input]
  (let [reduced-tiles (map reduce-tile input)
        only-1 (filter (fn [[_ n]] (= n 1)) (frequencies reduced-tiles))]
    (count only-1)))

(defn generate-neighbours [tile]
  #{(update tile :x inc)
    (update tile :x dec)
    (multiple-update tile [["y" inc] ["x" #(+ % 0.5)]])
    (multiple-update tile [["y" inc] ["x" #(- % 0.5)]])
    (multiple-update tile [["y" dec] ["x" #(+ % 0.5)]])
    (multiple-update tile [["y" dec] ["x" #(- % 0.5)]])})

(defn flip-black-tile? [state tile]
  (let [neighbours (generate-neighbours tile)
        black-neighbours (set/intersection state neighbours)
        n-black (count black-neighbours)]
    (or (= n-black 0) (> n-black 2))))

(defn flip-white-tile? [state tile]
  (let [neighbours (generate-neighbours tile)
        black-neighbours (set/intersection state neighbours)
        n-black (count black-neighbours)]
    (= n-black 2)))

(defn solve-2 [n input]
  (let [reduced-tiles (map reduce-tile input)
        only-1 (filter (fn [[_ n]] (= n 1)) (frequencies reduced-tiles))]
    (loop [state (into #{} (keys only-1))
           iteration 0]
      (if (= iteration n)
        (count state)                                               ;do something
        (let [kept-black-tiles (filter #(not (flip-black-tile? (into #{} state) %)) state)
              white-tiles (set/difference (reduce #(set/union %1 %2) (map generate-neighbours state)) state)
              flipped-white-tiles (filter (partial flip-white-tile? (into #{} state)) white-tiles)]
          (recur (concat kept-black-tiles flipped-white-tiles) (inc iteration)))))))

(defn -main
  [& args]
  (let [sol1 (comp solve-1 read-lines)
        sol2 (comp (partial solve-2 100) read-lines)]
    (pp/pprint (sol1 "inputday24-test.txt"))
    (pp/pprint (sol1 "inputday24.txt"))
    (pp/pprint (sol2 "inputday24-test.txt"))
    (pp/pprint (sol2 "inputday24.txt"))))
