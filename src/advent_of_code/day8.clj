(ns advent-of-code.day8
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn parse-input [line]
  (def match (re-matches #"^(acc|jmp|nop) (\+|\-)(\d+)$" line))
  {:op  (get match 1)
   :num (if (= (get match 2) "+")
          (Integer/parseInt (get match 3))
          (- 0 (Integer/parseInt (get match 3)))
          )
   :ex  false}
  )


(defn ex-prog [acc idx c-i insts]
  (def new-insts
    (merge insts {idx (merge c-i {:ex true})})
    )
  (if (or (get c-i :ex) (>= idx (count insts)))
    acc
    (case (get c-i :op)
      "nop" (ex-prog acc
                     (+ idx 1)
                     (get insts (+ idx 1))
                     new-insts)
      "jmp" (ex-prog acc
                     (+ idx (get c-i :num))
                     (get insts (+ idx (get c-i :num)))
                     new-insts)
      "acc" (ex-prog (+ acc (get c-i :num))
                     (+ idx 1)
                     (get insts (+ idx 1))
                     new-insts)
      ))
  )


(defn is-ex-prog [acc last-idx idx c-i insts]
  (def new-insts
    (merge insts {idx (merge c-i {:ex true})})
    )
  (if (and (= idx (count insts)) (= (- idx last-idx) 1))    ;if we are at last instruction + 1
    true
    (if (or (>= idx (count insts)) (get c-i :ex))           ;if the program can't execute anymore
      false
      (case (get c-i :op)
        "nop" (is-ex-prog acc
                          idx
                          (+ idx 1)
                          (get insts (+ idx 1))
                          new-insts)
        "jmp" (is-ex-prog acc
                          idx
                          (+ idx (get c-i :num))
                          (get insts (+ idx (get c-i :num)))
                          new-insts)
        "acc" (is-ex-prog (+ acc (get c-i :num))
                          idx
                          (+ idx 1)
                          (get insts (+ idx 1))
                          new-insts)
        ))))



(defn -main
  [& args]

  (def data-file
    (io/resource "inputday8.txt"))

  (def lines (str/split (slurp data-file) #"\n"))

  (def instructions
    (->>
      (map parse-input lines)
      (zipmap (range 0 (count lines))))
    )

  (println (ex-prog 0 0 (get instructions 0) instructions)) ;answer for first star

  (def jmp-nop-instructions
    (filter (fn [[l ins]] (or (= "jmp" (get ins :op)) (= "nop" (get ins :op)))) instructions)
    )

  (def new-progs (map (fn [[l ins]]
                        (merge
                          instructions
                          (case (get ins :op)
                            "jmp" {l {:op "nop" :ex false :num (get ins :num)}}
                            "nop" {l {:op "jmp" :ex false :num (get ins :num)}})))
                      jmp-nop-instructions))

  (def ok-prog (first
                 (filter (fn [prog] (is-ex-prog 0 0 0 (get prog 0) prog)) new-progs)
                 ))
  (println (ex-prog 0 0 (get ok-prog 0) ok-prog))           ;answer for second star
  )
