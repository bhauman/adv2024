(ns adv2024.grid
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo])
  (:import
   [clojure.lang PersistentQueue]))

(def transpose (partial apply mapv vector))

(defn coord-bounds [points]
  (->> (transpose points)
       (mapv sort)
       (mapv (juxt first last))))

#_(defn mod-pos [pos bounds]
    (mapv pos)
    )

(defn extents [points]
  (transpose (coord-bounds points)))

(defn in-bounds? [bounds p]
  (every? (fn [[p [l h]]]
            (<= l p h))
          (map vector p bounds)))

(defn on-edge? [bounds p]
  (first (filter
          (fn [[p [l h]]]
           (or (= l p)
               (= l h)))
         (map vector p bounds))))

(defn all-locations [bounds]
  (->> bounds
       (mapv #(range (first %) (inc (second %))))
       (apply combo/cartesian-product)
       (mapv vec)))

(defn all-locations-2d [bounds]
  (partition-by first (all-locations bounds)))

(defn all-locations-3d [point-set]
  (mapv
   #(partition-by second (mapv vec %))
   (partition-by first
                 (all-locations (coord-bounds point-set)))))

(defn points->matrix-rows-2d [point-set f]
  (let [point-set (cond-> point-set 
                    (map? point-set) keys)
        row-locations (all-locations-2d (coord-bounds point-set))]
    (mapv (partial mapv f) row-locations)))



(defn points->matrix-rows-3d [point-set f]
  (let [point-set (cond-> point-set 
                    (map? point-set) keys)
        mats-2d (all-locations-3d point-set)]
    #_mats-2d
    (mapv (partial mapv (partial mapv f)) mats-2d)))


(defn print-2d [matrix]
  (let [border (str "+" (apply str (repeat (count (first matrix)) "-"))
                    "+")]
    (println border)
    (doseq [row  matrix]
      (println (str "|" (apply str row) "|")))
    (println border)))

(defn print-3d [matrix]
  (println (str (apply str (repeat 10 "v"))
                " 3D Matrix START "
                (apply str (repeat 10 "v"))))
  (doseq [mat-2d matrix]
    (print-2d mat-2d))
  (println (str (apply str (repeat 10 "^"))
                " 3D Matrix END "
                (apply str (repeat 10 "^")))))
