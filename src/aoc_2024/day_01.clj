(ns aoc-2024.day-01
  (:require [clojure.string :as str]
            [aoc-2024.utils :as utils]))

(defn part-1 [left right]
  (loop [xs (seq left)
         index 0
         sum-of-differences 0]
    (if xs
      (let [x (first xs)]
        (recur
          (next xs)
          (inc index)
          (+ sum-of-differences (abs (- x (nth right index))))))
      sum-of-differences)))

(defn part-2 [left right]
  (->> right
       (filter (set left))
       (frequencies)
       (reduce-kv
         (fn [sum k v]
           (+ sum (* k v)))
         0)))

(defn day-1 []
  (let [lines (->> (utils/parse-file "input-01.txt")
                   (reduce (fn [acc row]
                             (-> acc
                                 (update 0 conj (first row))
                                 (update 1 conj (second row))))
                           [[] []]))
        [left right] (map sort lines)]
    (prn (part-1 left right))
    (prn (part-2 left right))))

(day-1)