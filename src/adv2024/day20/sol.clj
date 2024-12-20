(ns adv2024.day20.sol
  (:require
   [clojure.string :as string]
   [medley.core :as med]))

(def input (->> (slurp "src/adv2024/day20/input.txt")
                string/split-lines
                (mapv vec)))

(def width (count input))
(def track (->> (for [y (range width) x (range width)]
                  [[y x] (get-in input [y x])])
                (into {})))
(def start (ffirst (filter #(= \S (val %)) track)))
(def end (ffirst (filter #(= \E (val %)) track)))
(defn turn-right [[y x]] [x (- y)])
(def cardinals (take 4 (iterate turn-right [-1 0])))

(defn next-track [[prev p]]
  [p
   (->> (mapv #(mapv + p %) cardinals)
        (remove #(= % prev))
        (filter track)
        (filter #(not= \# (track %)))
        first)])

(def track-places (->> (iterate next-track [nil start])
                       (map second)
                       (med/take-upto #(= end %))
                       vec))

(def dist-from-start (into {} (map-indexed #(vector %2 %1) track-places)))

(defn manhat-dist [[^long y1 ^long x1] [^long y2 ^long x2]]
  (+ (abs (- y1 y2)) (abs (- x1 x2))))

(defn points-in-range [lim [y x :as p]]
  (for [ly (range (- y lim) (inc (+ y lim)))
        lx (range (- x lim) (inc (+ x lim)))
        :when (and
               (pos? ly)
               (pos? lx)
               (<= (manhat-dist p [ly lx]) lim)
               (dist-from-start [ly lx])
               (> (dist-from-start [ly lx])
                  (dist-from-start p)))]
    [ly lx]))

(defn cheats [max-depth p]
  (->> (points-in-range max-depth p)
       (map #(- (dist-from-start %) (dist-from-start p) (manhat-dist % p)))
       (remove zero?)))

;; part 1 test
#_(time
   (->> track-places
       (mapcat (partial cheats 2))
       frequencies
       (filter #(<= 100 (first %)))
       (map second)
       (reduce +)
       )) ; => 1448

;; part 2 test
#_(time
   (->> track-places
       (map (partial cheats 20))
       (apply concat)
       frequencies
       (filter #(<= 100 (first %)))
       (map second)
       (reduce +)
       )) ; => 1017615


