(ns advent-of-code.day19
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as combo])
  (:require [clojure.pprint :as pp]))

(defn read-lines [file-name]
  (let [res (io/resource file-name)]
    (str/split (slurp res) #"\n")))

(defn parse-rule [line]
  (let [[_ i refs] (re-matches #"(\d+): (((\d+\s?)+(\|\s)?)+)" line)]
    (if (not (nil? i))
      (let [or-refs (str/split refs #" \| ")
            ors (map
                  (fn [ands]
                    (let [indexes (str/split ands #"\s")]
                      (map #(Integer/parseInt %) indexes)))
                  or-refs)]
        [(Integer/parseInt i) {:type "ref" :ops ors}])
      (let [[_ i v] (re-matches #"(\d+): \"([a-z])\"" line)]
        [(Integer/parseInt i) {:type "value" :ops v}]))))



(defn parse-input [lines]
  (let [rules (map parse-rule (take-while #(not (empty? %)) lines))
        values (filter #(and (not (str/includes? % ":")) (not (empty? %))) lines)]
    {:rules  (into {} rules)
     :values values}))

(defn compute-vals [pos-vals]
  (loop [acc [""]
         p-vs pos-vals]
    (if (empty? p-vs)
      acc
      (let [new-acc (for [a acc
                          v (first p-vs)]
                      (str a v))]
        (recur new-acc (rest p-vs))))))

(defn generate-all [rules current-rule]
  (case (:type current-rule)
    "ref" (let [[left right] (:ops current-rule)
                left-pos-values (map #(generate-all rules (get rules %)) left)
                right-pos-values (map #(generate-all rules (get rules %)) right)]
            (filter #(not (empty? %)) (concat (compute-vals left-pos-values) (compute-vals right-pos-values))))
    "value" [(:ops current-rule)]))

(defn solve-1 [input]
  (let [rules (:rules input)
        pos-values (into #{} (generate-all rules (get rules 0)))
        values (:values input)]
    (count (filter #(contains? pos-values %) values))))


(defn solve-2 [input]
  (let [rules (:rules input)
        output-8 (str "(" (str/join "|" (map #(str "(" % ")") (generate-all rules (get rules 8)))) ")")
        output-42 (str "(" (str/join "|" (map #(str "(" % ")") (generate-all rules (get rules 42)))) ")")
        output-31 (str "(" (str/join "|" (map #(str "(" % ")") (generate-all rules (get rules 31)))) ")")
        rule-11-n (fn [n s]
                    (let [re-s-n (str output-8 "+" output-42 "{" n "}" output-31 "{" n "}")
                          re-11 (re-pattern re-s-n)]
                      (re-matches re-11 s)))
        rules-11-20 (map #(partial rule-11-n %) (range 1 21))
        rule-11-20 (fn [s] (reduce #(or %1 %2) (map #(% s) rules-11-20)))
        values (:values input)]
    (count (filter rule-11-20 values))))

(defn -main
  [& args]
  (let [sol1 (comp solve-1 parse-input read-lines)
        sol2 (comp solve-2 parse-input read-lines)]
    ;(pp/pprint (sol1 "inputday19-test.txt"))
    (pp/pprint (time (sol1 "inputday19.txt")))
    ;(pp/pprint (sol1 "inputday19-test2.txt"))
    (pp/pprint (time (sol2 "inputday19.txt")))
    )
  )
