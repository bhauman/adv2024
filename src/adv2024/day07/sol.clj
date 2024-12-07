(ns adv2024.day07.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [medley.core :as med]))

(def input (-> (slurp "src/adv2024/day07/input.txt")
               (string/replace ":" " ")
               string/split-lines
               (->> (mapv #(edn/read-string (str "[" % "]"))))))

(defn ev [[res init & args] ops]
  (= res
     (reduce (fn [acc [op n]] (op acc n)) init (map vector ops args))))

(defn solvable? [possible-ops calib]
  (->> (combo/selections possible-ops (- (count calib) 2))
       (filter #(ev calib %))
       first))

;; part 1
#_(->> (filter (partial solvable? [+ *]) input)
       (map first)
       (reduce +)) ;; => 1582598718861

(defn op-|| [a b]
  (parse-long (str a b)))

;; part 2
#_(def part2
    (->> (filter (partial solvable? [+ * op-||]) input)
         (map first)
         (reduce +))) ; => 165278151522644




