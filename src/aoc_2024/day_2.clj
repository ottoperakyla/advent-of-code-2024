(ns aoc-2024.day-2
  (:require [aoc-2024.utils :as utils]))

(def test-reports
  [[7 6 4 2 1]
   [1 2 7 8 9]
   [9 7 6 2 1]
   [1 3 2 4 5]
   [8 6 4 4 1]
   [1 3 6 7 9]])

(def real-reports
  (utils/parse-file "input-02.txt"))

(defn report->differences [report]
  (->> report
       (partition 2 1)
       (map (fn [[left right]] (- left right)))))

(defn safe-report? [report]
  (let [differences (report->differences report)]
    (and
      (or
        (every? neg? differences)
        (every? pos? differences))
      (every? #(<= (abs %) 3) differences))))

(defn part-1 [reports]
  (count (filter safe-report? reports)))

(defn report->variations [report]
  (cons
    report
    (map-indexed
      (fn [idx _level]
        (utils/remove-index idx report))
      report)))

(defn part-2 [reports]
  (count
    (filter
      #(some
         safe-report?
         (report->variations %))
      reports)))


(defn day-2 []
  (prn (part-1 real-reports))
  (prn (part-2 real-reports)))

(day-2)