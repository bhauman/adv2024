(ns adv2024.day04.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2024.util :as util]
   [adv2024.grid :as grid]))

(def input (->> (slurp "src/adv2024/day04/input.txt")
                (string/split-lines)
                (mapv (comp vec seq))))

(def width (count input))

(defn in-bounds? [p]
  (every? #(<= 0 % (dec width)) p))

(defn diagonal-poses [p]
  (take-while in-bounds? (iterate #(mapv inc %) p)))

(defn diag-starts []
  (let [res (mapv vector (repeat 0) (range width))]
    (distinct
     (into res (mapv (comp vec reverse) res)))))

(defn diags [in]
  (->> (map diagonal-poses (diag-starts))
       (map #(map (partial get-in in) %))))

(defn count-xmas [l]
  (let [s (apply str l)]
    (count (mapcat #(re-seq % s) [#"SAMX" #"XMAS"]))))

(defn count-all-xmas []
  (reduce +
          (map count-xmas
               (concat input
                       (grid/transpose input)
                       (diags input)
                       (diags (mapv (comp vec reverse) input))))))

;; part 1
(count-all-xmas) ;; => 2483


(defn corners [p]
  (let [res (mapv #(mapv + % p) [[-1 -1] [-1 1] [1 1] [1 -1]])]
    (when (every? in-bounds? res)
      res)))

(defn get-corners [p]
  (apply str (mapv #(get-in input %) (corners p))))

(defn locations [wid]
  (for [y (range wid)
        x (range wid)]
    [y x]))

(def x-mas? #{"MMSS" "SSMM" "MSSM" "SMMS"})

;; part 2
(count (filter
        #(and (= \A (get-in input %))
              (x-mas? (get-corners %)))
        (locations width))) ;; => 1925
