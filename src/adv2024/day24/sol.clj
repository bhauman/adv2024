(ns adv2024.day24.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [clojure.math :as math]
   [clojure.core.reducers :as r]
   [medley.core :as med]))

(def input (-> (slurp "src/adv2024/day24/input.txt")
               (string/replace "->" " ")
               (string/replace ":" " ")               
               string/split-lines
               (->> (mapv #(edn/read-string (str "[" % "]"))))))

(def init (into (sorted-map) (take-while not-empty input)))
(def ^:dynamic *gates* (->> input (drop-while not-empty) rest))

(defn run-gate [{:keys [st gates] :as state} [a f b out :as gate]]
  (or (when-let [av (get st a)]
        (when-let [bv (get st b)]
          {:st (assoc st out ((condp = f 'AND bit-and 'OR bit-or 'XOR bit-xor) av bv))
           :gates (remove #(= gate %) gates)}))
      state))

(defn step [{:keys [st gates] :as state}]
  (reduce run-gate state gates))

(defn run-state [state]
  (->> (iterate step state)
       (drop-while #(not-empty (:gates %)))
       first
       (:st)
       (filter #(string/starts-with? (str (key %)) "z"))
       (sort-by key)
       vals))

;; part 1
#_(->> (run-state {:st init :gates *gates*})
       reverse
       (apply str)
       (str "2r")
       edn/read-string) ; => 66055249060558

(def outputs
  (->> *gates*
       (map (fn [[in1 op in2 out]] [out [op in1 in2]]))
       (into {})))

(defn to-sym [sym n]
  (symbol (format (str sym "%02d") n)))

;; continuations based dsl for matching logic

(defn gat [op cont-r cont-l]
  (fn [g err out]
    (let [[op' l r] (get g out)]
      (if (= op' op)
        (or (and (cont-r g err r)
                 (cont-l g err l))
            (and (cont-r g err l)
                 (cont-l g err r)))
        (do (err [out :op-not op op']) false)))))

(defn term [l n]
  (fn [g err x] (= x (to-sym l n))))

(defn adder [n]
  (gat 'XOR 
       (gat 'XOR
            (term 'x n)
            (term 'y n))
       (gat 'OR
            (gat 'AND
                 (term 'x (dec n))
                 (term 'y (dec n)))
            (gat 'AND
                 (gat 'XOR
                      (term 'x (dec n))
                      (term 'y (dec n))) 
                 (constantly true) ;; cary in
                 ))))

(defn adder-test [outs n]
  (let [errs (atom nil)]
    (if-not ((adder n) outs #(swap! errs conj %) (to-sym 'z n))
      [n @errs])))

(defn swap-outs [outs n1 n2]
  (assoc outs n1 (get outs n2) n2 (get outs n1)))

(defn run-test [outs]
  (->> (range 44 1 -1)
       (keep (partial adder-test outs))))

(defn fix-step [{:keys [outs acc]}]
  (let [[[n1 bad-outs1] [n2 bad-outs2]] (run-test outs)]
    (when n1
      (let [[sw out]
            (->> (for [bo1 (map first bad-outs1)
                       bo2 (map first bad-outs2)
                       :let [nouts (swap-outs outs bo1 bo2)
                             [[r1] [r2]] (take 2 (run-test nouts))]
                       :when (every? (complement #{n1 n2}) [r1 r2])]
                   [[bo1 bo2] nouts])
                 first)]
        {:outs out :acc (conj acc sw)}))))

#_ (run-test outputs)

;; part 2
#_(->> (iterate fix-step {:outs outputs :acc []})
       (take-while not-empty)
       last
       :acc
       flatten
       sort
       (string/join ",")
       time)
            
;;=> fcd,fhp,hmk,rvf,tpc,z16,z20,z33




