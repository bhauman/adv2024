(ns adv2024.day11.sol
  (:require
   [clojure.edn :as edn]))

(def input (edn/read-string (str "(" (slurp "src/adv2024/day11/input.txt") ")")))

(defn blink [stone]
  (if (zero? stone)
    1
    (let [s (str stone) sz (count s)]
      (if (even? sz)
        [(parse-long (subs s 0 (/ sz 2))) (parse-long (subs s (/ sz 2)))]
        (* 2024 stone)))))

(def blink-count
  (memoize
   (fn [depth stone]
     (if (= 0 depth)
       1
       (let [stones (blink stone)]
         (if (number? stones)
           (blink-count (dec depth) stones)
           (+ (blink-count (dec depth) (first stones))
              (blink-count (dec depth) (second stones)))))))))

;; part 1
(reduce + (map (partial blink-count 25) input)) ; => 217812

;; part 2
(time
 (reduce + (map (partial blink-count 75) input)))  ; => 259112729857522


;; frequencies method
(defn hist-step [freq]
  (->> freq
       (mapcat (fn [[k prev-count]] (map (fn [c] {c prev-count}) (flatten [(blink k)]))))
       (apply merge-with +)))

#_(hist-step (frequencies [0 0 3]))

#_(->> (iterate hist-step (frequencies input))
       (drop 75)
       first
       vals
       (reduce +))


