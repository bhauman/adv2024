(ns adv2024.day24.logic
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.core.logic :as l :refer [fresh membero run run* conde]]))

(def input (-> (slurp "src/adv2024/day24/input.txt")
               (string/replace "->" " ")
               (string/replace ":" " ")
               string/split-lines
               (->> (mapv #(edn/read-string (str "[" % "]"))))))

(def gate-map (->> input
                   (drop-while not-empty)
                   rest
                   (map (juxt last butlast))
                   (into {})))

(defn to-sym [sym n] (symbol (format (str sym "%02d") n)))

(defn geto [m k o]
  (l/conda
   [(l/lvaro k)
    (l/membero [k o] (seq m))]
   [(l/project [k] (l/== (get m k) o))]))

(defn gate [out op a b]
  (l/conde
   [(geto gate-map out [a op b])]
   [(geto gate-map out [b op a])]))

(defn adder [f t1 t2 dt1 dt2 a0 a0' a1 a1' b0 b0' b1 b1' c0 c0']
  (l/fresh [carry-in]
    (gate f   'XOR a0  a1)
    (gate a0' 'XOR t1  t2)
    (gate a1' 'OR  b0  b1)
    (gate b0' 'AND dt1 dt2)
    (gate b1' 'AND c0  carry-in)
    (gate c0' 'XOR dt1 dt2)))

(defn get-swap [n]
  (run 1 [q]
    (fresh [f t1 t2 dt1 dt2 a0 a1 b0 b1 c0 p? q?]
      (l/== f (to-sym 'z n))
      (l/== t1 (to-sym 'x n))
      (l/== t2 (to-sym 'y n))
      (l/== dt1 (to-sym 'x (dec n)))
      (l/== dt2 (to-sym 'y (dec n)))
      (l/conda
       [(adder f  t1 t2 dt1 dt2 a0 a0 a1 a1 b0 b0 b1 b1 c0 c0)]
       ;; basically querying for discontinuities in the graph
       [(adder f  t1 t2 dt1 dt2 p? q? a1 a1 b0 b0 b1 b1 c0 c0)]
       [(adder f  t1 t2 dt1 dt2 a0 a0 p? q? b0 b0 b1 b1 c0 c0)]
       [(adder f  t1 t2 dt1 dt2 a0 a0 a1 a1 p? q? b1 b1 c0 c0)]
       [(adder f  t1 t2 dt1 dt2 a0 a0 a1 a1 b0 b0 p? q? c0 c0)]
       [(adder f  t1 t2 dt1 dt2 a0 a0 a1 a1 b0 b0 b1 b1 p? q?)]
       [(adder p? t1 t2 dt1 dt2 a0 a0 a1 a1 b0 b0 b1 b1 c0 c0)
        (l/== q? f)])
      (l/conda
       [(l/nonlvaro p?) (l/== q [p? q?])]
       [(l/lvaro p?) (l/== q false)]))))

#_(->> (range 45)  
       (mapcat get-swap)
       (filter identity)
       (reduce into [])
       distinct
       sort) ;; => (fcd fhp hmk rvf tpc z16 z20 z33)
