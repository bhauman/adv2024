(ns adv2024.day13.sol
  (:require
   [clojure.string :as string]))

(def input (->> (slurp "src/adv2024/day13/input.txt")
                string/split-lines
                (mapv #(map parse-long (re-seq #"\d{1,10}" %)))
                (filter not-empty)
                (partition 3)
                (map (fn [[d1 d2 p2]] [d1 (map - d2) p2]))))

(defn determinant [[dy1 dx1] [dy2 dx2]]
  (- (* dx1 dy2) (* dx2 dy1)))

(defn cramers [[y2 x2] [dy dx] det]
  (/ (- (* x2 dy) (* y2 dx)) det))

(defn intersection-steps [[dir1 dir2 p2]]
  (let [det (determinant dir1 dir2)]
    (when-not (zero? det)
      (let [res [(cramers p2 dir2 det) (cramers p2 dir1 det)]]
        (when-not (ratio? (first res))
          res)))))

;; part 1
#_(->> input
       (keep intersection-steps)
       (map (fn [[a b]] (+ (* 3 a) b)))
       (reduce +)) ;; => 24938

;; part 2
#_(->> input
       (map (fn [[d1 d2 p2]] [d1 d2 (map #(+ 10000000000000 %) p2)]))
       (keep intersection-steps)
       (map (fn [[a b]] (+ (* 3 a) b)))
       (reduce +)) ;; => 104958599303720


