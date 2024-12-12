(ns adv2024.day12.sol
  (:require
   [clojure.string :as string]
   [medley.core :as med]))

(def input (->> (slurp "src/adv2024/day12/input.txt")
                string/split-lines
                (mapv (comp vec seq))))

(def input-map (->> (for [y (range (count input))
                          x (range (count input))
                           :let [crop (get-in input [y x])]]
                       [[y x] crop])
                    (into {})))

(defn turn-right [[y x]] [x (- y)])
(def cardinals (take 4 (iterate turn-right [-1 0])))
(defn neighbors [p] (map #(mapv + p % )  cardinals))

(defn area-perim-members [p]
  (let [seen (atom #{})
        crop (get input-map p)]
    (->> (tree-seq #(and (= crop (input-map %))
                         (not (@seen %))
                         (swap! seen conj %))
                   #(remove @seen (neighbors %))
                   p)
         (group-by #(= crop (input-map %)))
         (#(mapv % [true false])))))

(defn all-area-perims-from [startp]
  (loop [points [startp]
         areas-seen #{}
         ap-accum []]
    (let [[p & ps] (->> points (filter input-map) (remove areas-seen))]
      (if (nil? p)
        ap-accum
        (let [[areap perimp :as ap] (area-perim-members p)
              next (->> perimp (filter input-map) (remove areas-seen))]
          [ap next]
          (recur (concat ps next)
                 (into areas-seen areap)
                 (conj ap-accum ap)))))))

; part 1
#_(time
   (->> (all-area-perims-from [0 0])
        (map (fn [[area perim]]
               (* (count (distinct area)) (count perim))))
        (reduce +))) ; => 1371306

(defn next-to [p1 p2]
  (= 1 (apply + (map abs (mapv - p1 p2)))))

(defn seperate-neighbors-step [[neighs points]]
  (let [new-ns (for [n neighs p points :when (next-to n p)] p)]
    [(concat neighs new-ns) (remove (set new-ns) points)]))

(defn seperate-neighbors [points]
  (->> (iterate seperate-neighbors-step [[(first points)] (rest points)])
       (partition 2)
       (drop-while #(apply not= %))
       ffirst))

(defn group-neighbors [cardinal-perims]
  (->> (iterate #(seperate-neighbors (second %)) [nil cardinal-perims])
       rest
       (med/take-upto #(empty? (second %)))
       (map first)))

(defn side-count [areas perims]
  (let [as (set areas)]
      (->> (map
            (fn [dir] (filter #(as (mapv + dir %)) (distinct perims)))
            cardinals)
           (map (comp count group-neighbors))
           (reduce +))))

;; part 2
#_(time
   (->> (all-area-perims-from [0 0])
        (map (fn [[area perim]]
               (* (count (distinct area)) (side-count area perim))))
        (reduce +))) ; => 805880

