(ns adv2024.day01.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input (->> (str "[" (slurp "src/adv2024/day01/input.txt") "]")
                edn/read-string
                (partition 2)
                (apply map vector)))

;; part 1
(->> input
     (map sort)
     (apply map vector)
     (map #(abs (apply - %)))
     (reduce +)) ; => 1873376

;; part 2
(let [[a b] input
      freq (frequencies b)]
  (->> a
       (map #(* % (get freq % 0)))
       (reduce +))) ; => 18997088
                                        
