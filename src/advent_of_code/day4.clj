(ns advent-of-code.day4
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set]))


(def validity-map
  {"byr" (fn [x] (and
                   (some? (re-find #"\d{4}" x))
                   (<= 1920 (Integer/parseInt x) 2002)))
   "iyr" (fn [x] (and
                   (some? (re-find #"\d{4}" x))
                   (<= 2010 (Integer/parseInt x) 2020)))
   "eyr" (fn [x] (and
                   (some? (re-find #"\d{4}" x))
                   (<= 2020 (Integer/parseInt x) 2030)))
   "hgt" (fn [x] (do
                   (def h-match (re-matches #"^(\d+)(cm|in)$" x))
                   (if (some? h-match)
                     (if (= (get h-match 2) "cm")
                       (<= 150 (Integer/parseInt (get h-match 1)) 193)
                       (<= 59 (Integer/parseInt (get h-match 1)) 76)
                       )
                     false)
                   ))
   "hcl" (fn [x] (some? (re-matches #"^#(\d|[a-f]){6}$" x)))
   "ecl" (fn [x] (some? (re-find #"(amb|blu|brn|gry|grn|hzl|oth)" x)))
   "pid" (fn [x] (some? (re-matches #"^(\d){9}$" x)))})


(defn -main
  [& args]

  (def data-file
    (io/resource "inputday4.txt"))


  (->>
    (str/split (slurp data-file) #"\n\n")
    (map (fn [x] (re-seq #"([a-z]+):([^\s]*)" x)))
    (map (fn [matches]
           (map (fn [match] (get match 1))
                (filter (fn [match]
                          (do
                            ;(println match)
                            (and
                              (contains? validity-map (get match 1))
                              (apply
                                (get validity-map (get match 1))
                                [(get match 2)])))) matches))))
    (map set)
    (map (fn [s] (clojure.set/intersection s #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})))
    (filter (fn [s] (= (count s) 7)))
    count
    println)

  ;(println (apply (get validity-map "byr") ["2002"]))
  ;(println (apply (get validity-map "byr") ["2003"]))
  ;(println (apply (get validity-map "hgt") ["60in"]))
  ;(println (apply (get validity-map "hgt") ["190cm"]))
  ;(println (apply (get validity-map "hgt") ["190in"]))
  ;(println (apply (get validity-map "hgt") ["190"]))
  ;(println (apply (get validity-map "hcl") ["#123abc"]))
  ;(println (apply (get validity-map "hcl") ["#123abz"]))
  ;(println (apply (get validity-map "hcl") ["123abc"]))
  ;(println (apply (get validity-map "ecl") ["brn"]))
  ;(println (apply (get validity-map "ecl") ["wat"]))
  ;(println (apply (get validity-map "pid") ["000000001"]))
  ;(println (apply (get validity-map "pid") ["0123456789"]))

  ;(println (first (str/split (slurp data-file) #"\n\n")))
  ;(->>
  ;  (slurp data-file)
  ;  (fn [x] (str/split x #"\r\n\rn"))
  ;  println)
  )
