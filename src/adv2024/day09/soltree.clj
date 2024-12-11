(ns adv2024.day09.soltree
  (:require
   [clojure.string :as string]
   [clojure.zip :as z]))

(def input (->> (slurp "src/adv2024/day09/input.txt")
                string/trim
                (map #(parse-long (str %)))
                (partition-all 2)
                (mapcat (fn [id [f-sz e-sz]] [[f-sz id] [(or e-sz 0)]]) (range))))

;; tree structure
;; leaf node is a vector [size-of-sector id]
;; branch node is (max-sector-size<updated on change> max-id [nodes ...] )
;; branch node is decorated with info to aid efficiently navigating to a node

(defn max-empty-sect [children]
  (->> children (map first) (apply max 0)))

(defn mk-node [mx-id chs]
  (conj chs mx-id
        (->> (if (vector? (first chs))
               (filter (comp nil? second) chs)
               chs)
             max-empty-sect)))

(defn init-node [chs]
  (-> (apply max 0 (keep second chs)) (mk-node chs)))

(defn level-size [sz depth]
  (Math/round (condp = depth
                1 (Math/sqrt sz)
                2 (Math/cbrt sz)
                3 (Math/sqrt (Math/sqrt sz)))))

(defn make-tree [input depth]
  (let [lvl-size (level-size (count input) depth)]
    (->> (iterate #(map init-node (partition-all lvl-size %))  input)
         (drop depth)
         first
         init-node)))

(defn into-zipper [in]
  (z/zipper #(not (vector? %))
            (comp rest rest)
            #(mk-node (second %1) %2)
            in))

;; I'm sure there is probably a better tree
(defn mk-leaf-retriever [enter-branch? my-leaf?]
  (fn [loc' arg]
    (loop [loc loc']
      (cond
        (nil? loc) false
        (z/end? loc) false
        ;; don't ever pass where last node was retrieved
        ;; this isn't needed but speeds things up a little
        ;; also can place this as third element
        (-> loc z/node meta ::end) false 
        (z/branch? loc)
        (if (enter-branch? loc arg)
          (recur (z/down loc))
          (recur (z/right loc)))
        :else ;; leaf
        (if (my-leaf? loc arg)
          loc
          (recur (z/right loc)))))))

(def retrieve-node-zip
  (mk-leaf-retriever (fn [loc n] (<= n (second (z/node loc))))
                     (fn [loc n] (= n (second (z/node loc))))))

(def find-blank-sect
  (mk-leaf-retriever (fn [loc sz] (<= sz (first (z/node loc))))
                     (fn [loc sz] (and (nil? (second (z/node loc)))
                                       (<= sz (first (z/node loc)))))))

(defn ztop [loc']
  (loop [loc loc']
    (let [zu (z/up loc)]
      (if (nil? zu) loc (recur zu)))))

(defn move-sector [dsk n]
  (let [spot (retrieve-node-zip dsk n)
        [sz id :as fl] (z/node spot)]
    (if-let [sect (-> spot
                      (z/edit #(vary-meta (vector (first %)) assoc ::end true))
                      ztop
                      (find-blank-sect sz))]
      (-> sect
          (z/insert-left fl)
          (z/edit update 0 - sz)
          ztop)
      dsk)))

;; part 2
#_(let [dsk (-> input (make-tree 3) into-zipper)]
    (time
     (->> (reduce move-sector dsk (range (second (z/node dsk)) 1 -1))
          (iterate z/next)
          (take-while (complement z/end?))
          (map z/node)
          (filter vector?)
          (mapcat (fn [[sz id]] (repeat sz id)))
          (map-indexed (fn [i v] (* i (or v 0))))
          (reduce +)))) ; => 6239783302560
