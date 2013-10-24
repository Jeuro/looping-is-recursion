(ns looping-is-recursion)

(defn power-helper [acc n k]
  (if (< k 1)
    acc
    (power-helper (* acc n) n (dec k))))

(defn power [base exp]
  (power-helper 1 base exp))

(defn last-helper [current a-seq]
  (if (empty? a-seq)
    current
    (last-helper (first a-seq) (rest a-seq))))

(defn last-element [a-seq]
  (last-helper nil a-seq))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         p pred
         s a-seq]
    (cond 
      (empty? s) nil
      (p (first s)) i
      :else (recur (inc i) p (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         items 0
         s a-seq]
    (if (empty? s)
      (/ sum items)
      (recur (+ sum (first s)) (+ items 1) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         s a-seq]
    (if (empty? s)
      a-set
      (recur (toggle a-set (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [x n
         fib1 0
         fib2 1]
    (if (= x 0)
      fib1
      (recur (dec x) fib2 (+ fib1 fib2)))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
         acc []]
    (cond
      (empty? s) acc
      (some #(= % (first s)) acc) acc
      :else (recur (rest s) (conj acc (first s))))))

