(ns adv2024.day22.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.math :as math]
   [clojure.core.reducers :as r]
   [medley.core :as med]))

(def input (as-> (slurp "src/adv2024/day22/input.txt") x
             (edn/read-string (str "[" x "]"))))

(def mix bit-xor)
(defn prune [x] (mod x 16777216))

(defn step1 [sn] (-> sn (* 64) (mix sn) prune))

(defn step2 [sn] (-> sn (/ 32) long (mix sn) prune))

(defn step3 [sn] (-> sn (* 2048) (mix sn) prune))

(defn next-sn [sn] (-> sn step1 step2 step3))

(defn nth-secret [n seed] (-> (iterate next-sn seed) (nth n)))

;; part 1
#_(->> (map (partial nth-secret 2000) input)
       (reduce +)) ; => 16299144133

(defn sequence-price-map [n seed]
  (->> (iterate next-sn seed)
       (take (inc n))
       (map #(mod % 10))
       (partition 2 1)
       (map (fn [[cost-a cost-b]]
              [cost-b (- cost-b cost-a)]))
       (partition 4 1)
       (map (fn [cost-diffs]
              [(map second cost-diffs) (first (last cost-diffs))]))
       (med/distinct-by first)
       (into {})))

;; these take time
(comment

  (def price-maps (mapv #(sequence-price-map 2000 %) input))

  (def sorted-keys
    (->> (mapcat keys price-maps)
         frequencies
         (sort-by second >)
         (map first)))

  ;; part 2
  (->> sorted-keys
       (take 100)
       (map (fn [k] (reduce unchecked-add (keep #(get % k) price-maps))))
       (reduce max)) ;; => 1896

  )

