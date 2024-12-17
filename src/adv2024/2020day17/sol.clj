(ns adv2024.day17.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.core.reducers :as r]
   [medley.core :as med]))

(def input (->> (slurp "src/adv2024/day17/input.txt")
                string/split-lines
                (mapv vec)))

(def init (set (for [y (range (count input))
                     x (range (count input))
                     :when (= \# (get-in input [y x]))]
                 [y x 0])))

(def ^:dynamic *directions* (vec (rest (combo/cartesian-product [0 1 -1] [0 1 -1] [0 1 -1]))))

(defn neigh-points [p] (mapv #(mapv + p %) *directions*))
(defn neighbors [s p] (keep s (neigh-points p)))

(defn rule [orig accum p]
  (let [active (count (neighbors orig p))]
    (cond-> accum
      (and (orig p) (not (<= 2 active 3))) (disj p)
      (and (not (orig p)) (= 3 active ))   (conj p))))

(defn step [s]
  (->> (into s (mapcat neigh-points s))
       (reduce (partial rule s) s)))

;; part 1
(->> init
     (iterate step)
     (drop 6)
     first
     count)

(def directions4 (vec (rest (combo/cartesian-product [0 1 -1] [0 1 -1] [0 1 -1] [0 1 -1]))))

;; part 2
(time
 (binding [*directions* directions4]
   (->> (set (map #(conj % 0) init))
        (iterate step)
        (drop 6)
        first
        count)))



