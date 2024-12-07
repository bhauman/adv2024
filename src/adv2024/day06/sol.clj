(ns adv2024.day06.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [medley.core :as med]))

(def input (->> (slurp "src/adv2024/day06/input.txt")
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
          [p new-dir])
        [next-p dir]))))

(def visited-path (->> (iterate (mover crates) [startp up])
                       (take-while (comp locations first))))

;; part 1
#_(->> spaces-visited (map first) distinct count) ; => 4663

(defn detect-loop?
  ([crate] (detect-loop? [startp up] #{} crate))
  ([start-p-dir seen' crate]
   (let [move (mover (conj crates crate))]
     (loop [p-dir start-p-dir
            seen seen']
       (let [next-p-dir (move p-dir)]
         (cond
           (not (locations (first next-p-dir))) false
           (seen next-p-dir) crate
           :else
           (recur next-p-dir (conj seen next-p-dir))))))))

;; part 2
#_(time
   (->> (disj (set (map first visited-path)) startp)
        (pmap detect-loop?)
        (filter identity)
        count
        )) ;; => 1530


;; faster version that follows the path, and instead of starting at the beginning each
;; it gets start-pos-dir from just before the obstacle and uses the path so far to construct
;; the seen, thus each iteration has less work to do

;; the important thing here is that you can not try and obstacle the
;; second time that you reach it on a path bc that state [obstacle [start dir]]] is probably
;; not reachable for the reason that you have already passed through the space where that
;; you are about to place the obstacle

#_(time
   (->> (reverse visited-path)
        (iterate rest)
        (take-while not-empty)
        reverse               ;; at this point we have a seq of growing paths
        ;; need to keep the first path with a unique location to use as an obstacle 
        (med/distinct-by ffirst)
        (remove #(= startp (ffirst %))) ;; don't use the start position as obstacle
        (pmap #(detect-loop? (second %) (into #{} (rest %)) (ffirst %)))
        (filter identity)
        distinct
        count
        )) ; => 1530

