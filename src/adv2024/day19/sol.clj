(ns adv2024.day19.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input (-> (slurp "src/adv2024/day19/input.txt")
               string/split-lines))

(def patterns (->> (str "(" (first input) ")")
                   edn/read-string
                   (map str)))

(def designs (->> input (drop 2)))

(defn comp-re [pats]
  (->> pats (reduce #(update-in %1 (vec %2) assoc :e true) {})))

(defn matcher [pat cur-p [x & xs :as s]]
  (cond
    (nil? cur-p) false
    (nil? x) (:e cur-p)
    :else (or
           (when (:e cur-p) (matcher pat pat s))
           (matcher pat (cur-p x) xs))))

(def re (comp-re patterns))

;; part 1
#_(->> (map (partial matcher re re) designs)
       (filter identity)
       count) ; => 220

(def match-count
  (memoize
   (fn [pat cur-p [x & xs :as s]]
     (cond
       (nil? cur-p) 0
       (nil? x) (if (:e cur-p) 1 0)
       :else
       (+
        (match-count pat (cur-p x) xs)
        (if (:e cur-p)
          (match-count pat pat s)
          0)))))) 

#_(time
   (->> designs
        (pmap (partial match-count re re))
        (reduce +))) ; => 565600047715343


