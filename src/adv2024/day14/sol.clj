(ns adv2024.day14.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [adv2024.grid :as grid]))

(def input (-> (slurp "src/adv2024/day14/input.txt")
               (string/replace #"=|p|v" "")
               (str ")")
               (->> (str "(")
                    edn/read-string
                    (partition 4))))

(def width  #_11 101)
(def height #_7  103)

(defn pos-at [n [x y dx dy]]
  [(mod (+ x (* n dx)) width)
   (mod (+ y (* n dy)) height)])

(defn quadrant [[x y]]
  (let [hmid (/ width 2)
        vmid (/ height 2)
        quad (cond-> []
               (< y (dec vmid)) (conj :n)
               (> y vmid)       (conj :s)               
               (< x (dec hmid)) (conj :e)
               (> x hmid)       (conj :w))]
    (when (= 2 (count quad)) quad)))

;; part 1
#_(->> (map (partial pos-at 100) input)
       (group-by quadrant)
       (remove (comp nil? first))
       vals
       (map count)
       (reduce *)) ; => 230436441

(defn print-points [ps]
  (let [points (set ps)]
    (-> points
        (grid/points->matrix-rows-2d #(if (points %) "*" " "))
        grid/transpose
        grid/print-2d)))

(def top-bottom-diff
  (memoize
   (fn [ps]
     (let [quads (group-by quadrant ps)]
       (- (+ (count (quads [:s :e])) (count (quads [:s :w])))
          (+ (count (quads [:n :e])) (count (quads [:n :w]))))))))

(def points-at-n (memoize (fn [n] (map (partial pos-at n) input))))

;; part 2

#_(let [initial (points-at-n 0)]
    (->> (range)
         (map points-at-n)
         rest
         (take-while #(not= initial %))
         (map-indexed vector)
         (filter #(> (top-bottom-diff (second %)) 173))
         (mapv (fn [[i ps]]
                 (prn (inc i))
                 (print-points ps))))) 

#_ (print-points (points-at-n 8270))

