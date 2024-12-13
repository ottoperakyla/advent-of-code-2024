(ns aoc-2024.day-11)

(def test-data
  [125 17])

(def real-data
  [0 89741 316108 7641 756 9 7832357 91])

(defn ->digits [n]
  (if (pos? n)
    (int (Math/floor (inc (Math/log10 n))))
    1))

(defn evolve-stones [stones]
  (loop [stones' stones
         result []]
    (if (seq stones')
      (let [stone (first stones')
            digits (->digits stone)]
        (recur
          (rest stones')
          (cond
            (= 0 stone)
            (conj result 1)

            (even? (->digits stone))
            (let [middle (/ digits 2)
                  divisor (Math/pow 10 middle)]
              (conj result
                    (int (quot stone divisor))
                    (int (rem stone divisor))))

            :else
            (conj result (* stone 2024)))))
      result)))

(def ->next-stones
  (memoize
    (fn [stone]
      (let [digits (->digits stone)]
        (cond
          (= 0 stone)
          [1]

          (even? digits)
          (let [middle (/ digits 2)
                divisor (Math/pow 10 middle)]
            [(int (quot stone divisor))
             (int (rem stone divisor))])

          :else
          [(* stone 2024)])))))

(defn evolve-stones-reduce [stones f]
  (reduce
    (fn [result stone]
      (concat result (f stone)))
    []
    stones))

(defn evolve-stones-lazy [stones]
  (lazy-seq
    (when-let [stone (first stones)]
      (let [[left right] (->next-stones stone)]
        (if right
          (cons left
                (cons right
                      (evolve-stones-lazy (rest stones))))
          (cons left
                (evolve-stones-lazy (rest stones))))))))

(defn evolve-stones-2 [stones n]
  (let [stones' (atom stones)]
    (dotimes [_k n]
      (swap! stones' #(evolve-stones-lazy %)))
    (count @stones')))

;; 22
#_(prn (count (nth (iterate evolve-stones test-data) 6)))
#_(prn (evolve-stones-2 test-data 6))
;; 55312
#_(prn (count (nth (iterate evolve-stones test-data) 25)))
#_(prn (evolve-stones-2 test-data 25))
;; part 1: 193899
#_(time (count (nth (iterate evolve-stones real-data) 25)))
#_(time (evolve-stones-2 real-data 25))

;; 193899
#_(prn (count (nth (iterate evolve-stones-lazy real-data) 25)))

;; Elapsed time: 24500.58075 msecs
#_(time (count (nth (iterate evolve-stones real-data) 40)))
;; Elapsed time: 19604.863417 msecs
#_(time (count (nth (iterate evolve-stones-lazy real-data) 40)))

;; "Elapsed time: 17275.427083 msecs"
;; => with transient "Elapsed time: 13785.728 msecs"
(time (evolve-stones-2 real-data 40))
#_(prn (evolve-stones-2 real-data 25))
;; part 2
#_(prn (count (nth (iterate evolve-stones-lazy real-data) 75)))
#_(prn (evolve-stones-2 real-data 75))