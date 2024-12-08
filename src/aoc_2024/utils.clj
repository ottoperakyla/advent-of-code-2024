(ns aoc-2024.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file [filename]
  (slurp (io/resource filename)))

(defn parse-file [filename]
  (->> (read-file filename)
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [numbers]
              (map
                #(Integer/parseInt %)
                numbers)))))

(defn remove-index [index coll]
  (let [vec-coll (vec coll)]
    (into
      (subvec vec-coll 0 index)
      (subvec vec-coll (inc index)))))

(defn get-middle-element [coll]
  (nth coll (Math/floor (/ (count coll) 2))))

(defn print-grid [grid]
  (doseq [row grid]
    (doseq [cell row]
      (print cell))
    (println)))

(defn out-of-bounds? [data row col]
  (or
    (> row (dec (count data)))
    (< row 0)
    (> col (dec (count (first data))))
    (< col 0)))

(defn mapv-indexed [f coll]
  (vec (map-indexed f coll)))