(ns adv2024.day16.sol
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]))

(def input (->> (slurp "src/adv2024/day16/input.txt")
                string/split-lines
                (mapv (comp vec seq))))

(def width (count input))
(def start [(- width 2) 1])
(def end (vec (reverse start)))

(defn turn-right [[y x]] [x (- y)])
(defn turn-left  [[y x]] [(- x) y])

(def maze
  (->> (combo/cartesian-product (range width) (range width))
       (remove #(= \# (get-in input %)))
       (map vec)
       (into #{})))

(defn next-steps [m p dir path-set]
  (->> [dir (turn-right dir) (turn-left dir)]
       (map #(vector (mapv + p %) %))
       (filter (comp m first))
       (remove (comp path-set first))))

(defn extend-path [m [sc [p dir] path-set]]
  (let [steps (next-steps m p dir path-set)]
    (map
     (fn [[np ndir]]
       [(+ sc (if (= ndir dir) 1 1001))
        [np ndir]
        (conj path-set np)])
     steps)))

(defn extend-paths [m path-maps]
  (let [paths (mapcat (partial extend-path m) (vals path-maps))
        num-paths (count paths)
        limit (int (* num-paths 0.75))]
    (->> paths
         (sort-by first)
         ;; tuned this for faster iterations on part 2
         (take (if (> limit 700) limit num-paths)) 
         (reduce
          (fn [accum [sc head path-set :as new-entry]]
            (update accum head
                    (fn [[old-sc _ old-path-set :as orig-entry]]
                      (cond
                        (nil? orig-entry) new-entry
                        (= sc old-sc) [sc head (into old-path-set path-set)]
                        (< sc old-sc) new-entry
                        :else orig-entry))))
          {}))))

;; part 1
#_(def best-score
    (time
     (->> (iterate (partial extend-paths maze)
                   {[start [0 1]] [0 [start [0 1]] #{start}]})
          (take-while not-empty)
          (mapcat #(select-keys % [[end [0 1]] [end [-1 0]]]))
          (take 6)
          (map val)
          (sort-by first)
          ffirst))) ; => 105508


;; part 2
#_(def num-best-seats
    (time
     (->> (iterate (partial extend-paths maze)
                   {[start [0 1]] [0 [start [0 1]] #{start}]})
          (take-while not-empty)
          (mapcat #(select-keys % [[end [0 1]] [end [-1 0]]]))
          (take 6)
          (map val)
          (sort-by first)
          first
          last
          count))) ; => 548




