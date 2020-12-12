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


(defn execute-prog-2 [instrs d wy wx dy dx]
  (if (empty? instrs)
    (+ (abs dy) (abs dx))
    (let [c-i (first instrs)
          i (:instr c-i)
          nm (:n c-i)]
      (case i
        \N (execute-prog-2 (rest instrs) d (+ wy nm) wx dy dx)
        \S (execute-prog-2 (rest instrs) d (- wy nm) wx dy dx)
        \E (execute-prog-2 (rest instrs) d wy (+ wx nm) dy dx)
        \W (execute-prog-2 (rest instrs) d wy (- wx nm) dy dx)
        \L (case nm
             90 (execute-prog-2 (rest instrs) d wx (- wy) dy dx)
             180 (execute-prog-2 (rest instrs) d (- wy) (- wx) dy dx)
             270 (execute-prog-2 (rest instrs) d (- wx) wy dy dx)
             (execute-prog-2 (rest instrs) d wy wx dy dx))
        \R (execute-prog-2 (conj (rest instrs)
                                 {:instr \L
                                  :n     (abs (- 360 nm))}) d wy wx dy dx)
        \F (execute-prog-2 (rest instrs) d wy wx (+ dy (* nm wy)) (+ dx (* nm wx)))
        ))
    )
  )

(defn -main
  [& args]
  (let [sol-1 (comp execute-prog (partial map parse-line) read-lines)
        lines (read-lines "inputday12.txt")
        input (map parse-line lines)]
    (println (sol-1 "inputday12.txt"))
    (println (execute-prog-2 input 0 1 10 0 0))
    )
  )
