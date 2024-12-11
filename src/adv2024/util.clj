(ns adv2024.util)

(defn bfs-leaves-seq* [branch? children q]
  (loop [q q]
    (let [head (peek q)]
      (cond
        (empty? q) nil
        (not (branch? head))
        (cons head (lazy-seq (bfs-leaves-seq* branch? children (pop q))))
        :else (recur (apply conj (pop q) (children head)))))))

(defn bfs-leaves-seq [branch? children root]
  (bfs-leaves-seq* branch? children (conj (clojure.lang.PersistentQueue/EMPTY) root)))

(defn wrap-seq [x] (str "(" x ")"))
(defn wrap-vec [x] (str "[" x "]"))
