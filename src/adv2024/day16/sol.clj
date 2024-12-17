(ns adv2024.day16.sol
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]
   [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]))

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

(defn next-nodes [m [p dir]]
  (->> [dir (turn-right dir) (turn-left dir)]
       (map #(vector (mapv + p %) %))
       (filter (comp m first))))

(defn step [m [unvisited visited]]
  (let [[[_ dir :as p-dir] sc] (first unvisited)
        neighs (->> (next-nodes m p-dir) (remove visited))]
    [(-> (reduce
          (fn [unv [np ndir :as neig]]
            (update unv neig #(min (+ sc (if (= ndir dir) 1 1001))
                                   (or % Integer/MAX_VALUE))))
          unvisited
          neighs)
         (dissoc p-dir))
     (assoc visited p-dir sc)]))

;; part 1
#_(time (->> [(priority-map [start [0 1]] 0) {}]
             (iterate (partial step maze))
             (take-while (comp not-empty first))
             last
             (#(select-keys (second %) [[end [0 1]] [end [-1 0]]]))
             (map val)
             sort
             first))


;; modify part 1 to collect the paths
(defn step2 [m [unvisited visited]]
  (let [[[_ dir :as p-dir] [sc path-set]] (first unvisited)
        neighs (->> (next-nodes m p-dir) (remove visited))]
    [(-> (reduce
          (fn [unv [np ndir :as neig]]
            (update unv neig
                    (fn [[old-sc old-path-set]]
                      (let [nsc (+ sc (if (= ndir dir) 1 1001))
                            n-path-set (conj path-set np)]
                        (cond
                          (nil? old-sc)  [nsc n-path-set]
                          (< nsc old-sc) [nsc n-path-set]                          
                          (= old-sc nsc) [nsc (into old-path-set n-path-set)]
                          :else [old-sc old-path-set])))))
          unvisited
          neighs)
         (dissoc p-dir))
     (assoc visited p-dir (val (first unvisited)))]))

;; part 2
#_(->> [(priority-map-keyfn first [start [0 1]] [0 #{start}])
        {}]
       (iterate (partial step2 maze))
       (take-while (comp not-empty first))
       last
       (#(select-keys (second %) [[end [0 1]] [end [-1 0]]]))
       (map val)
       (sort-by first)
       first
       second
       count)

