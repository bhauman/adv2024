(ns adv2024.day01.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input (edn/read-string (str "["(slurp "src/adv2024/day01/input.txt") "]")))

;; part 1
(->> input
     (partition 2)
     (apply map vector)
     (map sort)
     (apply map vector)
     (map #(abs (apply - %)))
     (reduce +)) ; => 1873376


;; part 2
(->> input
     (partition 2)
     (apply map vector)
     ((juxt first (comp frequencies second)))
     ((fn [[l freq]]
        (->> l
             (map #(* % (get freq % 0)))
             (reduce +))))) ; 18997088
