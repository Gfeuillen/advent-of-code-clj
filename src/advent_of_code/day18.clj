(ns advent-of-code.day18
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

(defn execute-simple-statements [line]
  (loop [l (apply str line)
         acc 0
         next-op +
         ]
    (let [[_ left op right] (re-matches #"(\d+)\s?(\+|\*)?(.*)" l)]
      (if (nil? left)
        acc
        (case op
          "+" (do
                (recur
                  (str/triml right)
                  (next-op acc (Long/parseLong left))
                  +))
          "*" (do
                (recur
                  (str/triml right)
                  (next-op acc (Long/parseLong left))
                  *))
          (next-op acc (Long/parseLong left)))))))

(defn execute-1 [line]
  (loop [current-line line]
    (let [[m expr] (re-find #"\(((\d+\s?(\+|\*)?\s?)+)\)" current-line)] ;match any parenthesis
      (if (nil? expr)
        (execute-simple-statements current-line)
        (recur (str/replace-first current-line m (str (execute-simple-statements expr))))))))


(defn execute-plus [line]
  (loop [current-line line]
    (let [regex "(\\d+\\s\\+\\s\\d+)"                       ;match any + statement
          [_ m] (re-find (re-pattern regex) current-line)]
      (if (nil? m)
        current-line
        (recur (str/replace-first current-line m (str (execute-simple-statements m))))))))

(defn execute-times [line]
  (loop [current-line line]
    (let [regex #"\(((\d+\s?\*?\s?)+)\)"                    ;match any * parenthesis
          [f-m m] (re-find (re-pattern regex) current-line)]
      (if (nil? m)
        current-line
        (recur (str/replace-first current-line f-m (str (execute-simple-statements m))))))))

(defn remove-parent [line]
  (loop [current-line line]
    (let [[m e] (re-find #"\((\d+)\)" current-line)]        ;match parenthesis with 1 element
      (if (nil? m)
        current-line
        (recur (str/replace-first current-line m e))))))

(defn execute-2 [line]
  (loop [current-line line]
    (let [no-plus (execute-plus current-line)
          no-tim (execute-times no-plus)
          no-par (remove-parent no-tim)]
      (if (or (str/includes? no-tim "(") (str/includes? no-tim "+")) ;if something prio to be executed
        (recur no-par)
        (execute-simple-statements no-tim)))))


(defn solve1 [lines]
  (apply + (map execute-1 lines)))

(defn solve2 [lines]
  (apply + (map execute-2 lines)))

(defn -main
  [& args]
  (let [sol1 (comp solve1 read-lines)
        sol2 (comp solve2 read-lines)]
    (println (sol1 "inputday18.txt"))
    (println (sol2 "inputday18.txt"))
    )
  )
