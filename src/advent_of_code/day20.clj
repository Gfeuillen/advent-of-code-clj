(ns advent-of-code.day20
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
    (slurp res)))

(defn take-inside [coll]
  (into [] (drop-last (rest coll)))
  )

(defn parse-tile [tile-lines]
  (let [[_ tile-num] (re-matches #"Tile (\d+):" (first tile-lines))]
    {:num     (Integer/parseInt tile-num)
     :top     (nth tile-lines 1)
     :bottom  (last tile-lines)
     :left    (apply str (map first (rest tile-lines)))
     :right   (apply str (map last (rest tile-lines)))
     :content (map #(apply str (take-inside %)) (take-inside (rest tile-lines)))}))

(defn flip-content [content]
  (map #(apply str (reverse %)) content))

(defn flip-tile [tile]
  {:num     (:num tile)
   :top     (str/reverse (:top tile))
   :bottom  (str/reverse (:bottom tile))
   :left    (:right tile)
   :right   (:left tile)
   :content (flip-content (:content tile))})

(defn rotate-content-90 [content]
  (let [n (count (first content))]
    (map (fn [x] (apply str (reverse (map #(nth % x) content)))) (range n))))

(defn rotate-90 [tile]
  {:num     (:num tile)
   :top     (str/reverse (:left tile))
   :bottom  (str/reverse (:right tile))
   :left    (:bottom tile)
   :right   (:top tile)
   :content (rotate-content-90 (:content tile))})

(def rotate-180 (comp rotate-90 rotate-90))

(def rotate-270 (comp rotate-90 rotate-90 rotate-90))

(defn parse-input [text]
  (let [tiles-text (str/split text #"\n\n")
        tiles-lines (map #(str/split % #"\n") tiles-text)]
    (map parse-tile tiles-lines)))

(defn tile-fits-1pos [state pos tile]
  (let [[x y] pos
        left-pos [(dec x) y]
        right-pos [(inc x) y]
        top-pos [x (inc y)]
        bottom-pos [x (dec y)]
        fits-left (if-let [left-tile (get state left-pos)]
                    (= (:right left-tile) (:left tile))
                    true)
        fits-right (if-let [right-tile (get state right-pos)]
                     (= (:left right-tile) (:right tile))
                     true)
        fits-top (if-let [top-tile (get state top-pos)]
                   (= (:bottom top-tile) (:top tile))
                   true)
        fits-bottom (if-let [bottom-tile (get state bottom-pos)]
                      (= (:top bottom-tile) (:bottom tile))
                      true)]
    (and fits-left fits-right fits-top fits-bottom)))


(defn rotate-tile-fits-1pos [state pos tile]
  (let [rotated [tile (rotate-90 tile) (rotate-180 tile) (rotate-270 tile)]
        flipped (concat rotated (map flip-tile rotated))
        rotated-fits (filter #(tile-fits-1pos state pos %) flipped)]
    (if-not (empty? rotated-fits)
      rotated-fits
      nil)))


(defn possible-assignments [state pos tile]
  (let [fitting-tiles (rotate-tile-fits-1pos state pos tile)]
    (map #(vector pos %) fitting-tiles)))


(defn possible-recur-args [state possible-assignments possible-pos tiles]
  (for [[pos tile] possible-assignments]
    (let [surrounding-pos [[(dec (first pos)) (second pos)]
                           [(inc (first pos)) (second pos)]
                           [(first pos) (inc (second pos))]
                           [(first pos) (dec (second pos))]]
          new-state (assoc state pos tile)
          new-state-pos (into #{} (keys new-state))
          new-possible-pos (filter #(not (contains? new-state-pos %)) (concat possible-pos surrounding-pos))]
      [new-state new-possible-pos (rest tiles)])))


(def find-sol-1
  (fn [state possible-pos tiles]
    (if (empty? tiles)
      (vector state)
      (let [tile (first tiles)
            assignments (mapcat #(possible-assignments state % tile) possible-pos)
            possible-args (possible-recur-args state assignments possible-pos tiles)
            [s p t] (first possible-args)]
        (if-not (empty? possible-args)
          (recur s p t)
          ;(mapcat #(apply find-sol-1 %) possible-args) ;generating too many times the same option -- if it fits, result is ok
          (recur state possible-pos (concat (rest tiles) [(first tiles)]))) ;test next tile
        ))))

(defn is-solution-square [solution]
  (let [indexes (into #{} (keys solution))
        top (apply max (map second indexes))
        bot (apply min (map second indexes))
        left (apply min (map first indexes))
        right (apply max (map first indexes))]
    (and
      (contains? indexes [left top])
      (contains? indexes [left bot])
      (contains? indexes [right top])
      (contains? indexes [right bot]))
    )
  )

(defn compute-mult [solution]
  (let [indexes (into #{} (keys solution))
        top (apply max (map second indexes))
        bot (apply min (map second indexes))
        left (apply min (map first indexes))
        right (apply max (map first indexes))]
    (*
      (:num (get solution [left top]))
      (:num (get solution [left bot]))
      (:num (get solution [right top]))
      (:num (get solution [right bot])))
    )
  )

(defn solve-1 [input]
  (let [solutions (find-sol-1 {[0 0] (first input)} [[0 1] [0 -1] [1 0] [-1 0]] (into [] (rest input)))
        solution (first (filter is-solution-square solutions))]
    solution
    )
  )

(defn assemble-map [sol1]
  (let [indexes (into #{} (keys sol1))
        top (apply max (map second indexes))
        bot (apply min (map second indexes))
        left (apply min (map first indexes))
        right (apply max (map first indexes))
        left-right (range left (inc right))
        top-bot (reverse (range bot (inc top)))
        columns (map (fn [x] (apply concat (map (fn [y] (:content (get sol1 [x y]))) top-bot))) left-right)
        n-lines (range (count (first columns)))]
    (map (fn [idx] (apply str (map #(nth % idx) columns))) n-lines)
    )
  )

;                  #
;#    ##    ##    ###
; #  #  #  #  #  #
(defn is-see-monster [full-map x y]
  (let [n (count (first full-map))
        pxls [[x (+ y 1)]
              [(+ x 1) (+ y 2)]
              [(+ x 4) (+ y 2)]
              [(+ x 5) (+ y 1)]
              [(+ x 6) (+ y 1)]
              [(+ x 7) (+ y 2)]
              [(+ x 10) (+ y 2)]
              [(+ x 11) (+ y 1)]
              [(+ x 12) (+ y 1)]
              [(+ x 13) (+ y 2)]
              [(+ x 16) (+ y 2)]
              [(+ x 17) (+ y 1)]
              [(+ x 18) (+ y 1)]
              [(+ x 18) y]
              [(+ x 19) (+ y 1)]]
        is-pxls-monster (map (fn [[x y]] (if (or (< (- n x) 1) (< (- n y) 1))
                                           false
                                           (let [e (nth (nth full-map y) x)]
                                             (= e \#)))) pxls)]
    (reduce #(and %1 %2) is-pxls-monster)
    )
  )

(defn find-monsters [full-map]
  (let [n (range (count (first full-map)))
        monsters (for [x n y n]
                   (if (is-see-monster full-map x y) 1 0))]
    (apply + monsters)))

(defn solve-2 [full-map]
  (let [full-map-90 (rotate-content-90 full-map)
        full-map-180 (rotate-content-90 full-map-90)
        full-map-270 (rotate-content-90 full-map-180)
        maps [full-map
              (flip-content full-map)
              full-map-90
              (flip-content full-map-90)
              full-map-180
              (flip-content full-map-180)
              full-map-270
              (flip-content full-map-270)]
        n-monster (apply max (map find-monsters maps))]
    (- (count (filter #(= % \#) (apply concat full-map))) (* 15 n-monster))
    )
  )


(defn -main
  [& args]
  (let [sol1 (comp solve-1 parse-input read-lines)
        sol2 (comp solve-2 assemble-map sol1)]
    ;(pp/pprint (compute-mult (sol1 "inputday20-test.txt")))
    (pp/pprint (compute-mult (sol1 "inputday20.txt")))
    ;(pp/pprint (sol2 "inputday20-test.txt"))
    (pp/pprint (sol2 "inputday20.txt"))
    )
  )
