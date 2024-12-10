(ns adv2024.day10.sol
  (:require
   [clojure.string :as string]))

(def input (->> (slurp "src/adv2024/day10/input.txt")
                string/split-lines
                (mapv #(mapv (comp parse-long str) %))))

(def width (count (first input)))

(def locations (set (for [y (range width) x (range width)] [y x])))

(def cardinals (take 4 (iterate (fn [[y x]] [x (- y)]) [-1 0])))

(defn at [p] (get-in input p))

(defn next-steps [p]
  (let [cur-val (at p)]
    (->> (mapv #(mapv + p %) cardinals)
         (filter locations)
         (filter #(= (inc cur-val) (at %))))))

(defn find-nines-from [p]
  (->> (tree-seq #(not= 9 (at %)) next-steps p)
       (filter #(= 9 (at %)))))

(def trail-heads (filter #(zero? (at %)) locations))

;; part 1
(reduce + (map (comp count distinct find-nines-from)
               trail-heads)) ; => 472

;; part 2
(reduce + (map (comp count find-nines-from)
               trail-heads)) ; => 969
