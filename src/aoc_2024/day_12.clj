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

(defn ->find [x parent]
  (loop [root x]
    (if (= (nth parent root) root)
      root
      (recur (nth parent root)))))

(defn ->merge [x y parent]
  (assoc parent
    (->find x parent)
    (->find y parent)))

(defn ->coordinate [index num-cols]
  [(quot index num-cols)
   (rem index num-cols)])

(defn ->array-index [row col num-cols]
  (+ (* row num-cols) col))

(defn ->components [parent num-cols]
  (vals
    (reduce
      (fn [acc [index x]]
        (update acc (->find x parent) conj
                (->coordinate index num-cols)))
      {}
      (zipmap (range) parent))))

(defn ->neighbours [row col]
  [[(dec row) col]
   [(inc row) col]
   [row (dec col)]
   [row (inc col)]])

(defn do-merges [parent merges]
  (reduce
    (fn [acc [origin [right under]]]
      (cond->>
        (->merge origin right acc)
        (some? under)
        (->merge origin under)))
    parent
    merges))

(defn ->price [dimensions]
  (->> dimensions
       (map #(* (first %) (second %)))
       (apply +)))

(defn ->merges [data]
  (let [num-rows (count data)
        num-cols (count (first data))]
    (for [row (range num-rows)
          col (range num-cols)
          :when (or
                  ;; cell to the right is the same region
                  (= (get-in data [row col])
                     (get-in data [row (inc col)]))
                  ;; cell under this one is the same region
                  (= (get-in data [row col])
                     (get-in data [(inc row) col])))
          :let [coordinate (->array-index row col num-cols)]]
      [coordinate
       (keep
         (fn [[r c]]
           (when (= (get-in data [row col])
                    (get-in data [r c]))
             (->array-index r c num-cols)))
         [[row (inc col)]
          [(inc row) col]])])))

(defn part-1 [data]
  (let [parent (vec (range (* (count data) (count (first data)))))
        merges (->merges data)
        parent' (do-merges parent merges)
        components (->components parent' (count (first data)))
        areas (map count components)
        perimeters (map
                     (partial apply +)
                     (map
                       (fn [component]
                         (map
                           (fn [[row col]]
                             (count
                               (filter
                                 (fn [[r c]]
                                   (or
                                     (utils/out-of-bounds? data r c)
                                     (not= (get-in data [r c])
                                           (get-in data [row col]))))
                                 (->neighbours row col))))
                           component))
                       components))
        dimensions (map vector areas perimeters)]
    (->price dimensions)))

(defn day-12 []
  (prn (part-1 test-data))
  (prn (part-1 test-data-2))
  (prn (part-1 test-data-3))
  (prn (part-1 real-data)))

(day-12)