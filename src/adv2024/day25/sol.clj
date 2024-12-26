(ns adv2024.day25.sol
  (:require
   [clojure.string :as string]))

(def input (->> (string/split-lines (slurp "src/adv2024/day25/input.txt"))
                (partition-by string/blank?)
                (filter #(< 1 (count %)))
                (map #(->> (apply mapv vector %)
                           (map (comp dec count (partial filter #{\#})))
                           (vector (ffirst %))))
                (group-by first)
                vals))

(defn try-keys [[kys locks]]
  (for [[_ k] kys
        [_ l] locks
        :when (every? #(<= % 5) (mapv + k l))]
    1))

;; part 1
#_(->> (try-keys input) count)







