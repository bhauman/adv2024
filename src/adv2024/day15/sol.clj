(ns adv2024.day15.sol
  (:require
   [clojure.string :as string]))

(def input (->> (slurp "src/adv2024/day15/input.txt")
               string/split-lines
               (mapv (comp vec seq))))

(def map-lines (vec (take-while not-empty input)))
(def move-inst (->> (drop-while not-empty input)
                    (apply concat)))

(defn turn-right [[y x]] [x (- y)])
(defn turn-left  [[y x]] [(- x) y])
(def directions (->> (take 4 (iterate turn-right [-1 0]))
                     (map vector [\^ \> \v \<])
                     (into {})))

(defn locations [lines]
  (for [y (range (count lines)) x (range (count (first lines)))] [y x]))

(defn robot-start [lines]
  (->> (locations lines)
       (filter #(= \@ (get-in lines %)) )
       first))

(defn mk-warehouse [lines]
  (->> (locations lines)
       (map #(vector % (get-in lines %)))
       (remove #(#{\. \@} (second %)))
       (into {})))

;; can fail
(defn try-move-over [from dir wm]
  (let [to (mapv + from dir)
        to-ch   (wm to)]
    (when (not= \# to-ch)
      (when-let [res (if (nil? to-ch) wm (try-move-over to dir wm))]
        (-> res
            (dissoc from)
            (assoc to (wm from)))))))

(defn move [[rb wm :as state] dirch]
  (let [dir (directions dirch)]
    (or
     (when-let [res (try-move-over rb dir wm)]
       [(mapv + rb dir) res])
     state)))

;; part 1
#_(->> (reduce move [(robot-start map-lines) (mk-warehouse map-lines)] move-inst)
       second
       (filter #(= \O (val %)))
       keys
       (map (fn [[a b]] (+ (* 100 a) b)))
       (reduce +)) ; => 1527563

(defn other-box-half-dir [to-ch [y _ :as dir]]
  (condp = [to-ch y]
    [\[ -1] (turn-right dir)
    [\]  1] (turn-right dir)
    [\[  1] (turn-left dir)
    [\] -1] (turn-left dir)))

(defn try-move-up-down [from dir wm]
  (let [to (mapv + from dir)
        to-ch   (wm to)]
    (when-not (= to-ch \#)
      (if (nil? to-ch)
        (-> wm (dissoc from) (assoc to (wm from)))
        (let [other-to (map + to (other-box-half-dir to-ch dir))]
          (when-let [res (try-move-up-down to dir wm)]
            (when-let [res (try-move-up-down other-to dir res)]
              (cond-> (dissoc res from)
                (wm from) (assoc to (wm from))))))))))

(defn move2 [[rb wm :as state] dirch]
  (let [dir (directions dirch)]
    (or
     (when-let [res ((if (#{\^ \v} dirch) try-move-up-down try-move-over)
                     rb dir wm)]
       [(mapv + rb dir) res])
     state)))

#_(let [map-lines2 (mapv
                   (comp vec (partial mapcat {\# [\# \#]
                                              \. [\. \.]
                                              \O [\[ \]]
                                              \@ [\@ \.]}))
                   map-lines)]
  (->> (reduce move2
               [(robot-start map-lines2) (mk-warehouse map-lines2)]
               move-inst)
         second
         (filter #(= \[ (val %)))
         keys
         (map (fn [[a b]] (+ (* 100 a) b)))
         (reduce +))
  )  ; => 1521635

