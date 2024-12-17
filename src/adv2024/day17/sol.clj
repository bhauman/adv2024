(ns adv2024.day17.sol
  (:require
   [clojure.string :as string]
   [clojure.core.reducers :as r]))

(def input (->> (slurp "src/adv2024/day17/input.txt")
                string/split-lines
                (mapcat #(map parse-long (re-seq #"\d{1,10}" %)))))

(def start-state {:a (nth input 0)
                  :b (nth input 1)
                  :c (nth input 2)
                  :pt 0
                  :prog (vec (drop 3 input))})

(defn combo [st op]
  (if (< op 4)
    op
    (condp = op
      4 (:a st)
      5 (:b st)
      6 (:c st))))

(defn dv [reg st op]
  (assoc st reg
         (long (/ (get st :a) (Math/pow 2 (combo st op))))))

(defn adv [st op] (dv :a st op))
(defn bdv [st op] (dv :b st op))
(defn cdv [st op] (dv :c st op))

(defn bxl [st op]
  (update st :b #(bit-xor % op)))

(defn bst [st op]
  (assoc st :b (mod (combo st op) 8)))

(defn jnz [st op]
  (if (zero? (:a st))
    st
    (assoc st :pt op)))

(defn bxc [st op]
  (update st :b #(bit-xor % (:c st))))

(defn out [st op]
  (update st :out (fnil conj []) (mod (combo st op) 8)))

(def insts [adv bxl bst jnz bxc out bdv cdv])

(defn do-op [{:keys [pt prog] :as st}]
  (when (and st (< pt (count prog)))
    (let [inst-code (nth prog pt)
          op (nth prog (inc pt))
          nst (update st :pt + 2)]
      ((insts inst-code) nst op))))

(defn run-prog [st]
  (->> (iterate do-op st)
       (take-while not-empty)
       last))

;; part 1
#_(->> (run-prog start-state)
       :out
       (string/join ","))

(defn try-at [n]
  (:out (run-prog (assoc start-state :a n))))

(defn binary-search [l r f target-comp]
  (loop [l l
         r r
         result nil]
    (if-not (<= l r)
      result
      (let [mid (quot (+ l r) 2)
            mid-val (f mid)]
        (condp = (target-comp mid-val)
          0  (recur l (dec mid) mid)
          -1 (recur (inc mid) r result)
          (recur l (dec mid) result))))))

(defn matching-digits-val [n n2]
  (reduce + (map #(if (= %1 %2)
                    (long (Math/pow 10 %3))
                    0) n n2 (range))))

(defn find-quine-step [progn [start end]]
  (let [rng-sz (- end start)]
    (if (<= rng-sz 10000)
      (->> (range start end)
           (map (juxt identity try-at))
           (filter (fn [[a b]] (= b progn)))
           ffirst)
      (let [step-size (long (/ (- end start) 10000))]
        (->> (range start end step-size)
             (pmap (juxt identity try-at))
             (map (fn [[a b]] [a (matching-digits-val progn b)]))
             (reduce (partial max-key last))
             first
             (#(vector (- % step-size) (+ % step-size))))))))

;; part 2
#_(time
   (let [progn (:prog start-state)
         start (binary-search 0 100000000000000
                              #(count (try-at %))
                              #(compare % (count progn)))
         end (binary-search 0 1000000000000000
                            #(count (try-at %))
                            #(compare % (inc (count progn))))]
     (->> (iterate (partial find-quine-step progn)
                   [start end])
          (map #(do (prn :step %) %))
          (drop-while #(not (number? %)))
          first))) ; => 202975183645226








