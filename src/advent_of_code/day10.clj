(ns advent-of-code.day10
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn parse-input [line]
  (Long/parseLong line)
  )


(defn find-jolt [adapters c-1 c-3]
  (if (< (count adapters) 2)
    {:c-1 c-1 :c-3 c-3}
    (if (= (- (second adapters) (first adapters)) 1)
      (find-jolt (rest adapters) (+ c-1 1) c-3)
      (if (= (- (second adapters) (first adapters)) 3)
        (find-jolt (rest adapters) c-1 (+ c-3 1))
        (find-jolt (rest adapters) c-1 c-3)
        )
      )
    )
  )

(defn find-poss-jolt-memo []
  (with-local-vars
    [find-poss (memoize                                     ;trick to use memoization inside a recursive function
                 (fn [adapters] (if (< (count adapters) 2)
                                  1
                                  (let [cur (first adapters)
                                        nexts (take 3 (rest adapters))
                                        paths (count (filter (fn [x] (<= (- x cur) 3)) nexts))
                                        new-ad (map (fn [x] (drop (+ x 1) adapters)) (range paths))
                                        ]
                                    (apply + (map find-poss new-ad)))
                                  )))]
    (.bindRoot find-poss @find-poss)
    @find-poss))

(defn -main
  [& args]

  (def data-file
    (io/resource "inputday10.txt"))

  (def lines (map parse-input (str/split (slurp data-file) #"\n")))

  (println (count lines))
  (println (count (set lines)))                             ; no double -> can sort

  (let [res (find-jolt (sort (conj lines 0)) 0 0)]
    (println (* (:c-1 res) (+ (:c-3 res) 1)))               ; first part
    )

  (println (sort (conj lines 0)))
  (let [res ((find-poss-jolt-memo) (sort (conj lines 0)))]
    (println res)                                           ;second part
    )
  )
