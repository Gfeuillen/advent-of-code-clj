(ns advent-of-code.day9
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn parse-input [line]
  (Long/parseLong line)
  )

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [more (cart (rest colls))
          x (first colls)]
      (cons x more))))

(defn subsets [n items]
  (cond
    (= n 0) '(())
    (empty? items) '()
    :else (concat (map
                    #(cons (first items) %)
                    (subsets (dec n) (rest items)))
                  (subsets n (rest items)))))


(defn n-sets [l n]
  (if (>= (count l) n)
    (cons (take n l) (n-sets (rest l) n))
    nil
    )
  )

(defn is-sum-of [e l p]
  (->>
    (subsets p l)
    (map (fn [x] (apply + x)))
    (filter (fn [x] (= x e)))
    empty?
    not)
  )

(defn is-valid-n [l n p]
  (if (is-sum-of (nth l n) (take n l) p)
    (is-valid-n (rest l) n p)
    (nth l n)
    )
  )

(defn -main
  [& args]

  (def data-file
    (io/resource "inputday9.txt"))

  (def lines (map parse-input (str/split (slurp data-file) #"\n")))

  (println lines)

  (def res1 (is-valid-n lines 25 2))                        ;solution to first part
  (println res1)

  (def sumsto (->>
    (range 2 (- (count lines) 1))
    (mapcat (fn [n] (map (fn [x] [(apply + x) x]) (n-sets lines n))))
    (filter (fn [[sum x]] (= sum res1)))
    first))

  (println (+ (apply min (get sumsto 1)) (apply max (get sumsto 1)))) ;solution to second part
  )
