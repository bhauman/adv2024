(ns adv2024.day21.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math :as math]))

(def input (->> (slurp "src/adv2024/day21/input.txt")
                string/split-lines
                (mapv #(apply str (interleave (repeat \space) (take 3 %))))
                (map #(edn/read-string (str "[" % " :A]")))))

(def k-map (->> (for [y (range 4) x (range 3)
                      :let [k (get-in [[7 8 9]
                                       [4 5 6]
                                       [1 2 3]
                                       [nil 0 :A]]
                                      [y x])]
                      :when k]
                  [k [y x]])
                (into {})))

(def k-map-nil-pos [3 0])

(def dir-k-map (->> (for [y (range 2) x (range 3)
                          :let [k (get-in [[nil :u :A]
                                           [:< :v :>]]
                                          [y x])]
                          :when k]
                      [k [y x]])
                    (into {})))

(def dir-k-map-nil-pos [0 0])

(def dir->sym {[1 0] :v [0 -1] :< [0 1] :> [-1 0] :u})

(defn directions [[fy fx] [ty tx]]
  (let [x (long (math/signum (- tx fx)))
        y (long (math/signum (- ty fy)))]
    (cond-> []
      (not (zero? x)) (conj [0 x])
      (not (zero? y)) (conj [y 0]))))

(def min-len-for
  (memoize
   (fn [nil-pos depth prev-dir-sym f t]
     (if (zero? depth)
       1
       (if (= f t)
         (min-len-for dir-k-map-nil-pos (dec depth) :A (dir-k-map prev-dir-sym) (dir-k-map :A))
         (some->> (directions f t)
                  (map #(vector % (mapv + f %)))
                  (remove #(= nil-pos (second %)))
                  (map (fn [[dir p]]
                         (+ (min-len-for dir-k-map-nil-pos (dec depth) :A (dir-k-map prev-dir-sym) (dir-k-map (dir->sym dir)))
                            (min-len-for nil-pos depth (dir->sym dir) p t))))
                  (reduce min)))))))

(defn min-len-for-code [depth code]
  (->> (cons :A code)
       (partition 2 1)
       (map (fn [[f t]] (min-len-for [3 0] (inc depth) :A (k-map f) (k-map t))))
       (reduce +)))

(defn scorer [depth code]
  (* (min-len-for-code depth code) (parse-long (apply str (take 3 code)))))

#_(reduce + (map (partial scorer 2) input)) ; => 157908

#_(time (reduce + (map (partial scorer 25) input))) ; => 196910339808654
