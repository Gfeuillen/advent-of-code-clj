(ns advent-of-code.day16
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r])
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as combo]))

(defn read-lines [file-name]
  (let [res (io/resource file-name)]
    (str/split (slurp res) #"\n")))

(defn parse-rule [line]
  (let [[_ field _ r1-low r1-up r2-low r2-up] (re-find #"(([a-z]| )+): (\d+)-(\d+) or (\d+)-(\d+)" line)
        cond1 #(<= (Integer/parseInt r1-low) % (Integer/parseInt r1-up))
        cond2 #(<= (Integer/parseInt r2-low) % (Integer/parseInt r2-up))]
    {:field field :f (fn [x] (or (cond1 x) (cond2 x)))}))

(defn parse-ticket [line]
  (map #(Integer/parseInt %) (str/split line #",")))

(defn parse-input [lines]
  (loop [rules []
         tickets []
         y-ticket nil
         input lines
         blank 0]
    (if (empty? input)
      {:rules rules :tickets tickets :y-ticket y-ticket}
      (let [line (first input)]
        (if (empty? line)
          (recur rules tickets y-ticket (drop 2 input) (inc blank))
          (case blank
            0 (recur (conj rules (parse-rule line)) tickets y-ticket (rest input) blank)
            1 (recur rules tickets (parse-ticket line) (rest input) blank) ; your ticket
            2 (recur rules (conj tickets (parse-ticket line)) y-ticket (rest input) blank) ; one of the ticket
            ))))))

(defn m-contains-error? [rules n]
  (every? false? (map #((:f %) n) rules)))

(defn solve-1 [input]
  (let [all-values (apply concat (:tickets input))
        errors (filter (partial m-contains-error? (:rules input)) all-values)]
    (apply + errors)))

(defn at-least-one? [rules n]
  (not (every? false? (map #((:f %) n) rules))))

(defn ticket-error-free? [rules ticket]
  (every? true? (map #(at-least-one? rules %) ticket)))

(defn find-matching-fields [rules items]
  (let [ok-rules (filter #(every? true? (map (fn [i] ((:f %) i)) items)) rules)]
    (map #(:field %) ok-rules)))

(defn solve-2 [input]
  (let [rules (:rules input)
        valid-tickets (filter (partial ticket-error-free? rules) (conj (:tickets input) (:y-ticket input)))
        columns (map                                        ;get nth element of each ticket
                  (fn [i] (into [] (map #(nth % i) valid-tickets)))
                  (range (count (:y-ticket input))))
        fields (map (partial find-matching-fields (:rules input)) columns)]
    (loop [assignments {}]                                  ;find assignments
      (if (= (count assignments) (count (:y-ticket input))) ;if everything was assigned
        (let [departure-assigments (filter (fn [[_ v]] (str/starts-with? v "departure")) assignments)
              departure-idx (map #(get % 0) departure-assigments)]
          (apply * (map #(nth (:y-ticket input) %) departure-idx))) ;compute multiplication
        (let [filtered-f (map-indexed (fn [i vs] [i (filter #(not (contains? (into #{} (vals assignments)) %)) vs)]) fields)
              [i vs] (first (filter (fn [[_ vs]] (= 1 (count vs))) filtered-f))]
          (recur (assoc assignments i (first vs))))))))     ;assign the next field that can only take 1 value


(defn -main
  [& args]
  (let [sol1 (comp solve-1 parse-input read-lines)
        sol2 (comp solve-2 parse-input read-lines)]
    (println (sol1 "inputday16-test.txt"))
    (println (time (sol1 "inputday16.txt")))
    (println (time (sol2 "inputday16.txt")))))
