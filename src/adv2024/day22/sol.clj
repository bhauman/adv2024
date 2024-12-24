(ns adv2024.day22.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [medley.core :as med]
   [clojure.core.reducers :as r]))

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
              [cost-b (unchecked-subtract cost-b cost-a)]))
       (partition 4 1)
       (map (fn [cost-diffs]
              [(map second cost-diffs) (first (last cost-diffs))]))
       (med/distinct-by first)
       (into {})))


;; these take time
(comment

  (def price-maps (pmap #(sequence-price-map 2000 %) input))

  (def sorted-keys
    (->> (r/map (comp frequencies keys) price-maps)
          (r/fold (partial merge-with unchecked-add))
          (sort-by second >)
          (mapv first)))

  ;; part 2
  (->> sorted-keys
       (take 100)
       (map (fn [k] (reduce unchecked-add (keep #(get % k) price-maps))))
       (reduce max)) ;; => 1896

  )

