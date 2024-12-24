(ns adv2024.day23.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.math :as math]
   [clojure.core.reducers :as r]
   [medley.core :as med]))

(def input (as-> "src/adv2024/day23/input.txt" x
             (slurp x)
             (string/replace x "-" " ")
             (str "[" x "]")
             (edn/read-string x)
             (partition 2 x)))

(def graph (->> input
                (mapcat (juxt identity reverse))
                (map #(hash-map (first %) #{(second %)}))
                (reduce (partial merge-with into ))))

(defn t? [x] (= \t (first (str x))))

(def t-nodes (->> input flatten (filter t?) distinct))
(def all-nodes
  (vec (concat t-nodes
               (->> input
                    flatten
                    (remove (set t-nodes))
                    distinct))))

(defn part1 []
   (let [tn (count t-nodes)
         nc (count all-nodes)]
      (for [x (range tn)
            :let [xn (nth all-nodes x)]
            y (range x nc)
            :let [yn (nth all-nodes y)]
            :when (get-in graph [xn yn])
            z (range y nc)
            :let [zn (nth all-nodes z)]
            :when (get-in graph [yn zn])
            :when (get-in graph [xn zn])]
        #{xn yn zn})))

;; part 1
#_(time (count (part1)))


(defn connected? [graph n1 n2]
  (get-in graph [n1 n2]))

(defn cliqued-to [graph clique n]
   (every? (partial connected? graph n) clique))

(def largest-clique
  (memoize
   (fn [graph nodes clique start]
     (if (= start (count nodes))
       clique
       (->> (range start (count nodes))
            (map (partial nth nodes))
            (map #(if (cliqued-to graph clique %)
                    (largest-clique graph nodes (conj clique %) (inc start))
                    clique))
            (reduce (partial max-key count)))))))

(def max-size (reduce max (map count (vals graph))))

;; part 2
#_(->> graph
       (map (fn [[n nds]]
              (largest-clique graph (vec nds) #{n} 0)))
       (filter #(= max-size (count %)))
       first
       sort
       time) ; => (da do gx ly mb ns nt pz sc si tp ul vl)



