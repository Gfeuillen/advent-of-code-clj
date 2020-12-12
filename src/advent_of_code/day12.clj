(ns advent-of-code.day12
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

(defn read-lines [file-name]
  (let [res (io/resource file-name)]
    (str/split (slurp res) #"\n")
    )
  )

(defn parse-line [line]
  (let [instr (first line)
        n (apply str (rest line))]
    {:instr instr
     :n     (Integer/parseInt n)})
  )

(defn abs [n]
  (max n (- n)))


(defn degree-to-dir [d]
  (let [m (mod d 360)
        md (if (>= m 0) m (- 360 m))]
    (case md
      0 \E
      90 \N
      180 \W
      270 \S
      ))
  )

(defn execute-prog [instrs]
  (loop [instructions instrs
         degrees 0
         change-y 0
         change-x 0]
    (if (empty? instructions)
      (+ (abs change-x) (abs change-y))
      (let [instruction (first instructions)
            rest-instr (rest instructions)
            num (:n instruction)]
        (case (:instr instruction)
          \N (recur rest-instr degrees (+ change-y num) change-x)
          \S (recur rest-instr degrees (- change-y num) change-x)
          \E (recur rest-instr degrees change-y (+ change-x num))
          \W (recur rest-instr degrees change-y (- change-x num))
          \L (recur rest-instr (+ degrees num) change-y change-x)
          \R (recur rest-instr (- degrees num) change-y change-x)
          \F (recur (conj rest-instr
                          {:instr (degree-to-dir degrees)
                           :n     num}) degrees change-y change-x)
          ))
      ))

  )


(defn execute-prog-2 [instrs]
  (loop [instructions instrs
         waypoint-y 1
         waypoint-x 10
         change-y 0
         change-x 0]
    (if (empty? instructions)
      (+ (abs change-y) (abs change-x))
      (let [instruction (first instructions)
            rest-instr (rest instructions)
            num (:n instruction)]
        (case (:instr instruction)
          \N (recur rest-instr (+ waypoint-y num) waypoint-x change-y change-x)
          \S (recur rest-instr (- waypoint-y num) waypoint-x change-y change-x)
          \E (recur rest-instr waypoint-y (+ waypoint-x num) change-y change-x)
          \W (recur rest-instr waypoint-y (- waypoint-x num) change-y change-x)
          \L (case num
               90 (recur rest-instr waypoint-x (- waypoint-y) change-y change-x)
               180 (recur rest-instr (- waypoint-y) (- waypoint-x) change-y change-x)
               270 (recur rest-instr (- waypoint-x) waypoint-y change-y change-x)
               (recur rest-instr waypoint-y waypoint-x change-y change-x))
          \R (recur
               (conj rest-instr {:instr \L :n (abs (- 360 num))})
               waypoint-y
               waypoint-x
               change-y
               change-x)
          \F (recur
               rest-instr
               waypoint-y
               waypoint-x
               (+ change-y (* num waypoint-y))
               (+ change-x (* num waypoint-x)))
          ))
      ))
  )

(defn -main
  [& args]
  (let [sol-1 (comp execute-prog (partial map parse-line) read-lines)
        sol-2 (comp execute-prog-2 (partial map parse-line) read-lines)]
    (println (sol-1 "inputday12.txt"))
    (println (sol-2 "inputday12.txt"))
    )
  )
