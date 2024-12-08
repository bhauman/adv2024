(ns adv2024.day08.better
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

(def width (count input))
(def locations (set (for [y (range width) x (range width)] [y x])))
(def station-locs (-> (group-by (partial get-in input) locations)
                      (dissoc \.)))

(defn antinodes [[a b]]
  (let [dydx (mapv - a b)]
    ;; (b + a - b) is 'a' so we want (a + a - b) and the opposite
    (filter locations [(mapv + a dydx) (mapv - b dydx)])))

;; part 1
(->> (vals station-locs)
     (map #(combo/combinations % 2))
     (map #(mapcat antinodes %))
     (reduce into #{})
     count) ;; => 273

(defn antinodes-freq [[a b]]
  (let [dydx (mapv - a b)]
    (into
     (take-while locations (iterate #(mapv - % dydx) a))
     (take-while locations (iterate #(mapv + % dydx) a)))))

;; part 2
(->> (vals station-locs)
     (map #(combo/combinations % 2))
     (map #(mapcat antinodes-freq %))
     (reduce into #{})
     count) ; => 1017



