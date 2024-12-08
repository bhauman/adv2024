(ns adv2024.day08.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.core.reducers :as r]
   [medley.core :as med]))

(def input (->> (slurp "src/adv2024/day08/input.txt")
                string/split-lines
                (mapv vec)))

#_(assert (= (count input) (count (first input))))
(def width (count input))
(def locations (set (for [y (range width) x (range width)] [y x])))
(def station-locs (-> (reduce #(update %1 (get-in input %2) conj %2) {} locations)
                      (dissoc \.)))

(defn dydx* [[[y x] [y' x']]] [(- y' y) (- x' x)])

(defn antinodes [station-pair]
  (let [dydx (dydx* station-pair)]
    (->> (concat
          (mapv #(mapv - % dydx) station-pair)
          (mapv #(mapv + % dydx) station-pair))
         (remove (set station-pair))
         (filter locations))))

;; part 1
(->> (vals station-locs)
     (map #(combo/combinations % 2))
     (map #(mapcat antinodes %))
     (reduce into #{})
     count) ;; => 273

(defn antinodes-freq [station-pair]
  (let [dydx (dydx* station-pair)]
    (into
     (take-while locations (iterate #(mapv - % dydx) (first station-pair)))
     (take-while locations (iterate #(mapv + % dydx) (first station-pair))))))

;; part 2
(->> (vals station-locs)
     (map #(combo/combinations % 2))
     (map #(mapcat antinodes-freq %))
     (reduce into #{})
     count) ; => 1017



