(ns adv2024.day09.soltreesimple
  (:require
   [clojure.string :as string]))

(def input (->> (slurp "src/adv2024/day09/input.txt")
                string/trim
                (map #(parse-long (str %)))
                (partition-all 2)
                (mapcat (fn [id [f-sz e-sz]] [[f-sz id] [(or e-sz 0)]]) (range))))

(defn max-empty-sect [children]
  (->> children (map first) (apply max 0)))

(defn mk-node [mx-id chs]
  (conj chs mx-id
        (->> (if (vector? (first chs))
               (filter (comp nil? second) chs)
               chs)
             max-empty-sect)))

(defn init-node [chs]
  (-> (apply max 0 (keep second chs)) (mk-node chs)))

(defn make-tree [input depth branch-node-size]
  (-> (iterate #(map init-node (partition-all branch-node-size %)) input)
      (nth depth)
      init-node))

(def branch? (complement vector?))
(def children (partial drop 2))

(defn insert-first [[x & xs] pred edit-fn]
  (cond
    (nil? x) nil
    (pred x) (edit-fn x xs)
    :else (cons x (insert-first xs pred edit-fn))))

(defn edit-first-node-id [tree n edit-fn]
  (if (empty? tree)
    false
    (mk-node
     (second tree)
     (if (branch? (first (children tree)))
       (insert-first
        (children tree)
        #(<= n (second %))
        #(cons (edit-first-node-id %1 n edit-fn) %2))
       (insert-first (children tree) #(= (second %) n) #(cons (edit-fn %1) %2))))))

(defn insert-id [tree [sz id :as nd]]
  (if (empty? tree)
    false
    (mk-node
     (second tree)
     (if (branch? (first (children tree)))
       (insert-first
        (children tree)
        #(<= sz (first %))
        #(cons (insert-id %1 nd) %2))
       (insert-first
        (children tree)
        (fn [[nsz nmax-id]] (and (<= sz nsz) (nil? nmax-id)))
        (fn [[nsz] xs] (cons nd (cons [(- nsz sz)] xs))))))))

(defn move-node [tree n]
  (let [found (atom nil)
        without-node
        (edit-first-node-id tree n #(do (reset! found %1) [(first %1)]))]
    (if @found
      (insert-id without-node @found)
      tree)))

#_(let [dsk (make-tree input 3 12)]
    (time
     (->> (reduce move-node dsk (range (second dsk) 1 -1))
          (tree-seq (complement vector?) children)
          (filter vector?)
          (mapcat (fn [[sz id]] (repeat sz id)))
          (map-indexed (fn [i v] (* i (or v 0))))
          (reduce +))))

