(ns aoc-2024.day-10
  (:require [aoc-2024.utils :as utils]
            [clojure.set :as set]))

(defn parse-input [filename]
  (utils/parse-grid
    filename
    (comp utils/parse-int str)))

(def test-data-simple
  (parse-input "input-10-simple-1.txt"))

(def test-data
  (parse-input "input-10-test.txt"))

(def real-data
  (parse-input "input-10-real.txt"))

(defn ->reachable-9-heights [trailhead [row col] grid]
  (cond
    (utils/out-of-bounds? grid row col)
    #{}

    (= 9 (get-in grid [row col]))
    #{[trailhead [row col]]}

    :else
    (let [value-here (get-in grid [row col])
          neighbours [[(dec row) col]
                      [row (inc col)]
                      [row (dec col)]
                      [(inc row) col]]]
      (apply set/union
             (->> neighbours
                  (filter (fn [[r c]] (= (inc value-here) (get-in grid [r c]))))
                  (map #(->reachable-9-heights trailhead % grid)))))))

(defn ->trailheads [grid]
  (for [row (range (count grid))
        col (range (count (first grid)))
        :when (= 0 (get-in grid [row col]))]
    [row col]))

(defn ->rating [trailhead [row col] grid]
  (cond
    (utils/out-of-bounds? grid row col)
    0

    (= 9 (get-in grid [row col]))
    1

    :else
    (let [value-here (get-in grid [row col])
          neighbours [[(dec row) col]
                      [row (inc col)]
                      [row (dec col)]
                      [(inc row) col]]]
      (->> neighbours
           (filter (fn [[r c]] (= (inc value-here) (get-in grid [r c]))))
           (map #(->rating trailhead % grid))
           (apply +)))))

(defn part-1 [grid]
  (->> (->trailheads grid)
       (map #(->reachable-9-heights % % grid))
       (apply set/union)
       (count)))

(defn part-2 [grid]
  (->> (->trailheads grid)
       (map #(->rating % % grid))
       (apply +)))

(defn day-10 []
  (prn (part-1 test-data-simple))
  (prn (part-1 test-data))
  (prn (part-1 real-data))
  (prn (part-2 test-data))
  (prn (part-2 real-data)))

(day-10)
