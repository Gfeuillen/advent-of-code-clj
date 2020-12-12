(ns advent-of-code.day12
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:require [clojure.core.reducers :as r]))

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

(defn execute-prog [instrs d dy dx]
  (println (list d dy dx))
  (if (empty? instrs)
    (+ (abs dy) (abs dx))
    (let [c-i (first instrs)
          i (:instr c-i)
          nm (:n c-i)]
      (case i
        \N (execute-prog (rest instrs) d (+ dy nm) dx)
        \S (execute-prog (rest instrs) d (- dy nm) dx)
        \E (execute-prog (rest instrs) d dy (+ dx nm))
        \W (execute-prog (rest instrs) d dy (- dx nm))
        \L (execute-prog (rest instrs) (+ d nm) dy dx)
        \R (execute-prog (rest instrs) (- d nm) dy dx)
        \F (execute-prog (conj (rest instrs)
                               {:instr (degree-to-dir d)
                                :n     nm}) d dy dx)
        ))
    )

  )


(defn execute-prog-2 [instrs d wy wx dy dx]
  (println (list d wy wx dy dx))
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
             (execute-prog-2 (rest instrs) d wy wx dy dx)
             )
        \R (execute-prog-2 (conj (rest instrs)
                                 {:instr \L
                                  :n     (abs (- 360 nm))}) d wy wx dy dx)
        \F (execute-prog-2 (rest instrs) d wy wx (+ dy (* nm wy)) (+ dx (* nm wx)))
        ))
    )
  )

(defn -main
  [& args]
  (let [input-file (io/resource "inputday12.txt")
        lines (str/split (slurp input-file) #"\n")
        input (map parse-line lines)]
    (println (execute-prog input 0 0 0))
    (println (execute-prog-2 input 0 1 10 0 0))
    )
  )
