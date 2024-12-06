(ns adv2024.day06.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]))

(def input (->> (string/replace (slurp "src/adv2024/day06/input.txt") "|" " ")
                (string/split-lines)
                (mapv (comp vec seq))))

(def width (count input))
(def locations (set (for [y (range width) x (range width)] [y x])))
(def startp (first (filter #(= \^ (get-in input %)) locations)))
(def crates (reduce (fn [acc l]
                      (cond-> acc
                        (= \# (get-in input l)) (conj l))) #{} locations))
(def up [-1 0])
(defn turn-right [[y x]] [x (- y)])

(defn mover [obstacles]
  (fn [[p dir]]
    (let [next-p (mapv + p dir)]
      (if (obstacles next-p)
        (let [new-dir (turn-right dir)]
          [(mapv + p new-dir) new-dir])
        [next-p dir]))))

;; part 1
(time
 (->> (iterate (mover crates) [startp up])
      (map first)
      (take-while locations)
      distinct
      count)) ; => 4663

;; part 2

(defn detect-loop? [crate]
  (let [move (mover (conj crates crate))]
    (loop [p-dir [startp up]
           seen #{}]
      (let [next-p-dir (move p-dir)]
        (cond
          (not (locations (first next-p-dir))) false
          (seen next-p-dir) true
          :else
          (recur next-p-dir (conj seen next-p-dir)))))))

#_(def res (let [path (->> (iterate (mover crates) [startp up])
                           (map first)
                           (take-while locations)
                           set)]
             (->> (disj path startp)
                  ; (map-indexed #(do (prn %1) %2)) ; print progress
                  (filter detect-loop?))))

#_(count res) ;; => 1430   <-- Says this number is too low 


