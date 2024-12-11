(ns adv2024.day09.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.core.reducers :as r]
   [medley.core :as med]))

(def input (->> (slurp "src/adv2024/day09/input.txt")
                string/trim
                (map #(parse-long (str %)))))

(defn expander [in]
  (->> in
       (partition-all 2)
       (map-indexed vector)
       (mapcat (fn [[id [filen en]]] (concat (repeat filen id) (when en (repeat en false)))))
       vec))

(defn compact [front-idx end-idx disk]
  (if (= front-idx end-idx)
    disk
    (cond
      (not (nth disk end-idx)) (recur front-idx (dec end-idx) disk)
      (nth disk front-idx)     (recur (inc front-idx) end-idx disk)
      :else
      (recur front-idx (dec end-idx) (assoc disk
                                            front-idx (nth disk end-idx)
                                            end-idx false)))))

;; part 1
#_(let [in (expander input)]
    (->> (compact 0 (dec (count in)) in)
         (take-while identity)
         (map-indexed #(* %1 %2))
         (reduce +))) ;; => 6211348208140

(defn expander-part2 [in]
  (->> in
       (partition-all 2)
       (map-indexed
        (fn [id [filen en]]
          (vector (vec (repeat filen id)) (or en 0))))
       vec))

(defn update-first [coll pred? f]
  (loop [acc (transient [])
         [head & ds :as disk] coll]
    (cond
      (empty? disk) false
      (pred? head)
      (persistent! (reduce conj! (reduce conj! acc (f head)) ds))
      :else (recur (conj! acc head) ds))))

(defn find-and-insert [disk new-data]
  (update-first
   disk
   #(<= (count new-data) (second %))
   (fn [[data ne]] (vector [data 0] [new-data (- ne (count new-data))]))))

(defn retrieve-node [disk n]
  (let [idx (->> (range n (count disk))
                 (filter #(= n (ffirst (nth disk %))))
                 first)
        [data ne] (nth disk idx)]
    [data (assoc disk idx [[] (+ (count data) ne)])]))

(defn defrager [ds]
  (reduce
   (fn [disk n]
     (let [[insert-data ds] (retrieve-node disk n)]
       (or (find-and-insert ds insert-data) disk)))
   ds (range (dec (count ds)) 1 -1 )))

;; part 2
#_(def part2
    (time
     (let [ds (expander-part2 input)]
       (->> (defrager ds)
            (mapcat (fn [[data empty]] (concat data (repeat empty 0))))
            (map-indexed #(* %1 %2))
            (reduce +))))
    ) ;; => 6239783302560
