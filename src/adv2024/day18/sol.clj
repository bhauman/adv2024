(ns adv2024.day18.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.data.priority-map :refer [priority-map]]))

(def input (as-> (slurp "src/adv2024/day18/input.txt") x
             (str "(" x ")")
             (edn/read-string x)
             (partition 2 x)
             (map reverse x)))

(def width (if (< (count input) 30) 7 71))
(def start [0 0])
(def end [(dec width) (dec width)])
(def start-size (if (< width 10) 12 1024))

(defn turn-right [[y x]] [x (- y)])
(def cardinals (take 4 (iterate turn-right [-1 0])))

(defn next-nodes [p] (map #(mapv + p %) cardinals))

(defn maze [in]
  (->> (combo/cartesian-product (range width) (range width))
       (remove (set in))
       set))

(defn step [m [[[p sc] & _ :as unvisited] visited]]
  (let [neighs (->> (next-nodes p) (filter m) (remove visited))]
    [(reduce
      #(update %1 %2 (fn [osc] (min (inc sc) (or osc Long/MAX_VALUE))))
      (dissoc unvisited p)
      neighs)
     (assoc visited p sc)]))

(defn get-path-length [m]
  (->> [(priority-map start 0) {}]
       (iterate (partial step m))
       (filter (comp empty? first))
       first
       second
       (#(get % end))))

;; part 1
#_(time (get-path-length (maze (take start-size input))))

; part 2
#_(def res
    (time
     (->> (drop start-size input)
          (reductions
           (fn [[_ m] n] [n (disj m n)])
           [nil (maze (take start-size input))])
          (filter (comp nil?
                        #(time (get-path-length %))
                        second))
          ffirst
          reverse)))





