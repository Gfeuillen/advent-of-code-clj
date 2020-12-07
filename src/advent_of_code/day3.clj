(ns advent-of-code.day3
  (:gen-class)
  (:require [clojure.java.io :as io]))


(def data-file
  (io/resource "inputday3.txt"))

(defn find-with-slope [x, y, lines, posx, posy, n-tree, wrap-n]
  (if (empty? lines)
    (do
      n-tree)
    (do
      (def line
        (if (or (= posy 0) (= (count lines) 1))
          (do
            (println "hitting condition")
            (first lines))
          (first (drop (- y 1) lines))))
      (def is-tree
        (= \# (get line posx)))
      (def next-pos-x
        (mod (+ x posx) wrap-n))
      (def next-pos-y
        (+ y posy))
      ;(println "round")
      ;(println is-tree)
      ;(println next-pos-x)
      (if is-tree
        (find-with-slope x y (drop y lines) next-pos-x next-pos-y (+ n-tree 1) wrap-n)
        (find-with-slope x y (drop y lines) next-pos-x next-pos-y n-tree wrap-n)
        )
      )
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def lines
    (line-seq (clojure.java.io/make-reader data-file [])))
  (println
    (reduce *
            (vector
              ;(find-with-slope 1 1 lines 0 0 0 (count (first lines)))
              ;(find-with-slope 3 1 lines 0 0 0 (count (first lines)))
              ;(find-with-slope 5 1 lines 0 0 0 (count (first lines)))
              ;(find-with-slope 7 1 lines 0 0 0 (count (first lines)))
              (find-with-slope 1 2 lines 0 0 0 (count (first lines))))))
  ;(println (find-with-slope 1 1 lines 0 0 0 (count (first lines))))
  ;(println (find-with-slope 3 1 lines 0 0 0 (count (first lines))))
  ;(println (find-with-slope 5 1 lines 0 0 0 (count (first lines))))
  ;(println (find-with-slope 7 1 lines 0 0 0 (count (first lines))))
  ;(println (find-with-slope 1 2 lines 0 0 0 (count (first lines))))
  )
