(ns advent-of-code.day21
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

(defn parse-line [line]
  (let [[_ ingredients _ allergens] (re-matches #"((\w+\s)+)\(contains ((\w+,?\s?)+)\)" line)]
    {:ingredients (str/split ingredients #"\s")
     :allergens   (str/split allergens #",\s")}))


(defn parse-lines [lines]
  (let [parsed (map parse-line lines)
        all-ingredients (apply concat (map :ingredients parsed))]
    [(frequencies all-ingredients)
     (r/fold
       hash-map
       (fn [acc v]
         (let [ing (:ingredients v)]
           (r/fold (constantly acc)
                   (fn [a all]
                     (let [current (get a all all-ingredients)]
                       (assoc a all (set/intersection (into #{} current) (into #{} ing)))
                       ))
                   (:allergens v))
           ))
       parsed)]))

(defn solve-1 [[ing-freq alle-to-ing]]
  (let [comb (into [] (map #(into #{} %) (vals alle-to-ing)))
        safe (reduce #(set/difference %1 %2) (concat [(into #{} (keys ing-freq))] comb))]
    (apply + (map #(get ing-freq %) safe))))


(defn solve-2 [input]
  (loop [assignments (get input 1)]
    (let [assigned (filter #(= (count (get % 1)) 1) assignments)
          assigned-values (apply concat (vals assigned))
          not-assigned (filter #(> (count (get % 1)) 1) assignments)
          not-assigned-filtered (map (fn [[k v]] [k (set/difference v (into #{} assigned-values))]) not-assigned)]
      (if (empty? not-assigned)
        (str/join "," (map #(first (get % 1)) (sort assigned)))
        (recur (merge (into {} assigned) (into {} not-assigned-filtered)))))))

(defn -main
  [& args]
  (let [sol1 (comp solve-1 parse-lines read-lines)
        sol2 (comp solve-2 parse-lines read-lines)]
    (pp/pprint (sol1 "inputday21-test.txt"))
    (pp/pprint (sol1 "inputday21.txt"))
    (pp/pprint (sol2 "inputday21-test.txt"))
    (pp/pprint (sol2 "inputday21.txt"))))
