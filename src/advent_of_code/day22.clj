(ns advent-of-code.day22
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
    (str/split (slurp res) #"\n\n")))

(defn parse-player [player]
  (into [] (map #(Integer/parseInt %) (rest (str/split player #"\n")))))

(defn parse-input [players]
  (map parse-player players))

(defn death-match [[p1 p2]]
  (if (or (empty? p1) (empty? p2))
    (concat p1 p2)
    (let [c1 (first p1)
          c2 (first p2)]
      (if (> c1 c2)
        (recur [(concat (rest p1) [c1 c2]) (rest p2)])
        (recur [(rest p1) (concat (rest p2) [c2 c1])])))))

(defn compute-sol-1 [player]
  (apply + (map-indexed #(* (inc %1) %2) (reverse player))))

(defn inside-recursive-game [input-mem player-1 player-2]
  (if (contains? input-mem [player-1 player-2])
    {:winner "p1" :p1 player-1 :p2 player-2}
    (let [c1 (first player-1)
          c2 (first player-2)
          r1 (rest player-1)
          r2 (rest player-2)
          new-mem (conj input-mem [player-1 player-2])]
      (cond
        (nil? c1) {:winner "p2" :p1 player-1 :p2 player-2}
        (nil? c2) {:winner "p1" :p1 player-1 :p2 player-2}
        :else (if (and (>= (count r1) c1) (>= (count r2) c2))
                (let [sub-game-res (inside-recursive-game input-mem (take c1 r1) (take c2 r2))]
                  (if (= (:winner sub-game-res) "p1")
                    (recur new-mem (concat r1 [c1 c2]) r2)
                    (recur new-mem r1 (concat r2 [c2 c1]))
                    )
                  )
                (if (> c1 c2)
                  (recur new-mem (concat r1 [c1 c2]) r2)
                  (recur new-mem r1 (concat r2 [c2 c1])))
                )))))

(defn recursive-game [[p1 p2]]
  (inside-recursive-game #{} p1 p2))

(defn select-winner-cards [recursive-game-res]
  ((keyword (:winner recursive-game-res)) recursive-game-res))

(defn -main
  [& args]
  (let [sol1 (comp compute-sol-1 death-match parse-input read-lines)
        sol2 (comp compute-sol-1 select-winner-cards recursive-game parse-input read-lines)]
    ;(pp/pprint (sol1 "inputday22-test.txt"))
    (pp/pprint (sol1 "inputday22.txt"))
    ;(pp/pprint (sol2 "inputday22-test.txt"))
    ;(pp/pprint (sol2 "inputday22-test2.txt"))               ;verifying no infinite loop
    (pp/pprint (sol2 "inputday22.txt"))))
