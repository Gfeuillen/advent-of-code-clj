(ns advent-of-code.day15
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as combo]))

(defn infinite-play [mem current i]
  (if (not (contains? mem current))
    (let [new-mem (assoc mem current i)]
      (lazy-seq (cons 0 (infinite-play new-mem 0 (inc i)))))
    (let [last-seen-i (get mem current)
          diff (- i last-seen-i)
          new-mem (assoc mem current i)]
      (lazy-seq (cons diff (infinite-play new-mem diff (inc i))))
      )
    )
  )

(defn sol-1 [input n]
  (if (<= n (count input))
    (get input (dec n))
    (nth
      (infinite-play
        (apply merge (map-indexed (fn [i v] {v i}) (drop-last input)))
        (last input)
        (dec (count input)))
      (- n (inc (count input)))))
  )

(defn -main
  [& args]
  (println (sol-1 [1 3 2] 2020))
  (println (sol-1 [2 1 3] 2020))
  (println (sol-1 [1 2 3] 2020))
  (println (sol-1 [20 0 1 11 6 3] 2020))                    ;part 1
  (println (sol-1 [20 0 1 11 6 3] 30000000))                ;part 2, same solution works
  )
