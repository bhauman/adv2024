(ns adv2024.day02.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input (slurp "src/adv2024/day03/input.txt"))


;; part1
(->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)
     (map rest)
     (map #(map edn/read-string %))
     (map #(apply * %))
     (reduce +)
     ) ;; => 170778545

;; part 2

(->> (re-seq #"don\'t\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\)" input)
     (reduce
      (fn [{:keys [doing accum] :as a} [com val1 val2]]
        (if (nil? val1)
          (assoc a :doing (= com "do()"))
          (cond-> a
            doing (update :accum + (* (Integer/parseInt val1) (Integer/parseInt val2))))))
      {:doing true
       :accum 0})
     ) ;; => 82868252
