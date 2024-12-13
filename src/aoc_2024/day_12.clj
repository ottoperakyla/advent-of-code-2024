(ns aoc-2024.day-12
  (:require [aoc-2024.utils :as utils]
            [clojure.set :as set]))

(def test-data
  (utils/parse-grid "input-12-simple.txt"))

(def test-data-2
  (utils/parse-grid "input-12-simple-2.txt"))

(def test-data-3
  (utils/parse-grid "input-12-simple-3.txt"))

(def test-data-3-modified
  (utils/parse-grid "input-12-simple-4.txt"))

(def real-data
  (utils/parse-grid "input-12-real.txt"))

(def default-inc (fnil inc 0))

(defn flood-fill [region row col data seen]
  (cond
    (utils/out-of-bounds? data row col)
    1

    (contains? @seen [row col])
    0

    (not= region (get-in data [row col]))
    1

    :else
    (do
      (swap! seen conj [row col])
      (+
        (flood-fill region (inc row) col data seen)
        (flood-fill region (dec row) col data seen)
        (flood-fill region row (dec col) data seen)
        (flood-fill region row (inc col) data seen)))))

(defn part-1 [data]
  #_(let [cells (for [row (range (count data))
                      col (range (count (first data)))]
                  [row col])]
      (loop [cells' (seq cells)
             result {:areas      {}
                     :perimeters {}}]
        (if cells'
          (let [cell (first cells')]
            (recur
              (next cells')
              result))
          result)))


  (let [areas (atom {})
        perimeters (atom {})]

    (doall
      (for [row (range (count data))
            col (range (count (first data)))]
        (let [seen (atom #{})]
          (swap! areas update (get-in data [row col]) default-inc)
          (swap! perimeters
                 update
                 (get-in data [row col])
                 (fn [d]
                   (set/union d
                              #{(flood-fill
                                  (get-in data [row col])
                                  row
                                  col
                                  data
                                  seen)}))))))


    ;A region of C plants with price 14 * 28 = 392. (420)
    ;A region of C plants with price 1 * 4 = 4. (420)

    ;A region of E plants with price 13 * 18 = 234. ok

    ;A region of F plants with price 10 * 18 = 180. ok

    ;A region of I plants with price 4 * 8 = 32. (396)
    ;A region of I plants with price 14 * 22 = 308. (396)

    ;A region of J plants with price 11 * 20 = 220. ok

    ;A region of M plants with price 5 * 12 = 60. ok

    ;A region of R plants with price 12 * 18 = 216. ok

    ;A region of S plants with price 3 * 8 = 24. ok

    ;A region of V plants with price 13 * 20 = 260. ok

    #_(prn "areas" @areas)
    #_(prn "perimeters" @perimeters)

    (let [prices (reduce
                   (fn [acc [region area]]
                     (assoc acc region (* area (apply + (vec (get @perimeters region))))))
                   {}
                   @areas)]
      #_(prn "prices" prices)
      (apply + (vals prices))
      )))

(defn day-12 []
  ;; 140
  #_(prn (part-1 test-data))
  ;; 772
  #_(prn (part-1 test-data-2))

  ;; expected 1930, but got 2010
  ;; getting too high on this one, but
  ;; too low on real data (???)
  ;; 2214 with code using sets
  (prn (part-1 test-data-3))

  ; OOXO
  ; OOXO

  ; "areas"       {"O" 6, "X" 2}
  ; "perimeters"  {"O" 6, "X" 6} <- O pitäisi olla 8 + 6 = 14
  ; "prices"      {"O" 36, "X" 12}

  #_(prn (part-1 test-data-3-modified))

  ;; 789328 is too low
  ;; 11711724 is too high
  (prn (part-1 real-data)))

(day-12)