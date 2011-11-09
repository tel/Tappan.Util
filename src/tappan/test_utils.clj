(ns tappan.test-utils)

(def EPS 1e-20)

(defn near
  "Returns true if the two values are close to one another."
  ([x y]
     (let [diff (- x y)
           diff (if (neg? diff) (- diff) diff)]
       (< diff EPS)))
  ([x y & more]
     (let [vals (distinct (conj more y x))]
       (every? (partial apply near) (for [a vals b vals :when (<= a b)] [a b])))))

(defn near<
  "Returns true if the sequence of arguments is nearly monotonic"
  ([x y]
     (< (- x y) EPS))
  ([x y & more]
     (let [vals (conj more y x)]
       (every? (partial apply near<) (map list vals (rest vals))))))