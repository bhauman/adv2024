(ns adv2024.day02.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input
  (->> (slurp "src/adv2024/day02/input.txt")
       string/split-lines
       (map #(str "[" % "]"))
       (map edn/read-string)))

(defn safe? [r]
  (let [ds (->> (partition 2 1 r) (map #(apply - %)) distinct)]
    (and
     (or (every? pos? ds) (every? neg? ds))
     (every? #(<= 1 % 3) (map abs ds)))))

(def part1 (->> input
                (filter safe?)
                count)) ;; => 202

(defn holey [r]
  (cons r (map #(concat (take % r)
                        (drop (inc %) r))
               (range (count r)))))

(defn safe-by-1 [l]
  (first (filter safe? (holey l))))

(def part2 (count (keep safe-by-1 input))) ;; => 271




