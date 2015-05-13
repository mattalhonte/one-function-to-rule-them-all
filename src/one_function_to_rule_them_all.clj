(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (reduce (fn [x y] (str x " " y)) ""  a-seq))

(defn my-interpose [x a-seq]
  (let [my-switch (fn [a b]
                    (if (= a [])
                      (conj a b)
                      (conj a x b)))]
  (reduce my-switch [] a-seq)))

(defn my-count [a-seq]
  (let [my-counter (fn [cur-count elem]
                     (if elem 
                     (inc cur-count)))]
  (reduce my-counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [rev-maker (fn [new-seq elem]
                    (cons elem new-seq))]
    (reduce rev-maker "" a-seq)))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [my-min (reduce min a-seq)
          my-max (reduce max a-seq)]
      [my-min my-max])))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    [n]
   (let [my-greaters (filter (fn [x] (> x n)) sorted-seq)
         my-lessers (filter (fn [x] (< x n)) sorted-seq)]
     (concat my-lessers (cons n my-greaters)))))

(defn insertion-sort [a-seq]
  (reduce insert []  a-seq))

(defn parity [a-seq]
  (let [parity-check (fn [x my-seq]
                       (let [my-set (set my-seq)]
                        (if (contains? my-set x)
                          (disj my-set x)
                          (conj my-set x))))]
    (reduce parity-check #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))


(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map [f a-seq]
  [:-])
