(ns advent-of-code.day14
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

(defn parse-bitmask-1 [line]
  (apply
    comp
    (let [[_ raw-mask] (re-find #"mask = ([X|1|0]{36,36})" line)]
      (map-indexed
        (fn [i, b]
          (case b
            \X identity                                     ;do nothing
            \1 (fn [value] (bit-set value i))               ;set bit at index
            \0 (fn [value] (bit-clear value i))))           ;clear bit at index
        (reverse raw-mask)))))                              ;needs to be reversed (right to left)

(defn parse-mem [line]
  (let [[_ i v] (re-find #"mem\[(\d+)\] = (\d+)" line)]
    [(Integer/parseInt i) (Integer/parseInt v)]))

(defn sol-1 [input]
  (loop [lines input
         mask identity
         mem {}]
    (if (empty? lines)
      (reduce + (vals mem))
      (let [line (first lines)]
        (if (str/starts-with? line "mask")
          (recur (rest lines) (parse-bitmask-1 line) mem)
          (let [[addr value] (parse-mem line)]
            (recur (rest lines) mask (assoc mem addr (mask value)))))))))

(defn parse-bitmask-2 [line]
  (let [[_ raw-mask] (re-find #"mask = ([X|1|0]{36,36})" line)]
    raw-mask))

(defn apply-bitmask [mask addr]
  (loop [m mask
         a addr
         acc (list)]
    (if (empty? a)
      (concat (reverse acc) m)
      (if (= (first m) \0)
        (recur (rest m) (rest a) (conj acc (first a)))
        (recur (rest m) (rest a) (conj acc (first m)))))))

(defn apply-selection [masked-addr selection]
  (loop [s selection
         acc (apply str (drop-while #(= \0 %) masked-addr))]
    (if (empty? s)
      acc
      (recur (rest s) (str/replace-first acc #"X" (first s))))))

(defn gen-mem [masked-addr]
  (let [x-count (count (filter #(= \X %) masked-addr))
        selections (combo/selections ["1" "0"] x-count)]
    (map
      (fn [selection]
        (Long/parseLong
          (apply-selection masked-addr selection)
          2))
      selections)))

(defn update-all [m ks v]
  (if (empty? ks)
    m
    (recur (assoc m (first ks) v) (rest ks) v)))

(defn sol-2 [input]
  (loop [lines input
         mask nil
         mem {}]
    (if (empty? lines)
      (reduce + (vals mem))
      (let [line (first lines)]
        (if (str/starts-with? line "mask")
          (recur (rest lines) (parse-bitmask-2 line) mem)
          (let [[addr value] (parse-mem line)
                masked-addr (reverse (apply-bitmask (reverse mask) (reverse (Integer/toString addr 2))))
                mem-addrs (gen-mem (apply str masked-addr))]
            (recur (rest lines) mask (update-all mem mem-addrs value))))))))

(defn -main
  [& args]
  (let [sol1 (comp sol-1 read-lines)
        sol2 (comp sol-2 read-lines)]
    (println (sol1 "inputday14-test.txt"))
    (println (sol1 "inputday14.txt"))
    (println (sol2 "inputday14-test2.txt"))
    (println (sol2 "inputday14.txt"))
    )
  )
