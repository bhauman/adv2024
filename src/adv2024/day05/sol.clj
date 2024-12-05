(ns adv2024.day05.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input-lines (->> (string/replace (slurp "src/adv2024/day05/input.txt") "|" " ")
                      (string/split-lines)
                      (map #(edn/read-string (str "[" % "]")))
                      (split-with (complement empty?))))

(def updates (rest (second input-lines)))
(def orders (first input-lines))
(def after (reduce (fn [acc [k v]] (update acc k (fnil conj #{}) v)) {} orders))
(defn after? [b a] (boolean (get-in after [b a])))

(defn valid-update? [[x & xs :as u]]
  (or (empty? u)
      (and (every? #(after? x %) xs)
           (valid-update? (rest u)))))

(defn middle [x] (nth x (int (/ (count x) 2))))

;; part 1
(->> updates
     (filter valid-update?)
     (map middle)
     (reduce +))

;; part 2
(->> updates
     (remove valid-update?)
     (map #(sort after? %))
     (map middle)
     (reduce +))



