(ns adv2024.day24.logic
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [medley.core :as med]
   [clojure.core.logic :as l :refer [fresh membero run run* conde]]))

(def input (-> (slurp "src/adv2024/day24/input.txt")
               (string/replace "->" " ")
               (string/replace ":" " ")               
               string/split-lines
               (->> (mapv #(edn/read-string (str "[" % "]"))))))

(def gate-map (->> gates (map (juxt last butlast)) (into {})))

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

(defn adder [f t1 t2 dt1 dt2 a1 a a1' a' b1 b b1' b' c1 c]
  (l/fresh [carry-in]
    (gate f  'XOR a a')
    (gate a1  'XOR t1 t2)
    (gate a1' 'OR b b')
    (gate b1  'AND dt1 dt2)
    (gate b1' 'AND c carry-in)
    (gate c1  'XOR dt1 dt2)))

(defn get-swap [n]
  (run 1 [q]
    (fresh [f f1 t1 t2 dt1 dt2 a1 a a' b b' c p_? q_?]
      (l/== f (to-sym 'z n))
      (l/== t1 (to-sym 'x n))
      (l/== t2 (to-sym 'y n))
      (l/== dt1 (to-sym 'x (dec n)))
      (l/== dt2 (to-sym 'y (dec n)))
      (l/conda
       [(simple-add3 f t1 t2 dt1 dt2 a a a' a' b b b' b' c c)]
       [(simple-add3 f t1 t2 dt1 dt2 p_? q_? a' a' b b b' b' c c)]
       [(simple-add3 f t1 t2 dt1 dt2 a a p_? q_? b b b' b' c c)]
       [(simple-add3 f t1 t2 dt1 dt2 a a a' a' p_? q_? b' b' c c)]
       [(simple-add3 f t1 t2 dt1 dt2 a a a' a' b b p_? q_? c c)]
       [(simple-add3 f t1 t2 dt1 dt2 a a a' a' b b b' b' p_? q_?)]
       [(simple-add3 p_? t1 t2 dt1 dt2 a a a' a' b b b' b' c c)
        (l/== q_? f)])
      (l/conda
       [(l/nonlvaro p_?) (l/== q [p_? q_?])]
       [(l/lvaro p_?) (l/== q false)]))))

(time
 (->> (range 1 45)
      (mapcat get-swap)
      (filter identity)
      (reduce into [])
      distinct
      sort))


#_([34 ([fcd :op-not OR XOR])]
   [33 ([z33 :op-not XOR OR])]
   [28 ([tpc :op-not AND XOR] [ntn :op-not XOR OR])]
   [27 ([rvf :op-not XOR AND] [dkb :op-not XOR OR])]
   [21
    ([fhp :op-not AND XOR] [fhp :op-not AND XOR] [vnk :op-not XOR OR])]
   [20 ([z20 :op-not XOR AND])]
   [17 ([fmr :op-not XOR OR] [hmk :op-not AND XOR])]
   [16 ([z16 :op-not XOR AND])])


;;=> fcd,fhp,hmk,rvf,tpc,z16,z20,z33
