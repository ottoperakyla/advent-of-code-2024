(ns aoc-2024.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-file [filename]
  (slurp (io/resource filename)))

(defn parse-file [filename]
  (->> (read-file filename)
       (string/split-lines)
       (map #(string/split % #"\s+"))
       (map (fn [numbers]
              (map
                #(Integer/parseInt %)
                numbers)))))

(defn parse-grid
  ([filename]
   (parse-grid filename str))
  ([filename f]
   (->> (read-file filename)
        (string/split-lines)
        (mapv
          #(mapv f (seq %))))))

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

(defn swap [index-a index-b coll]
  (-> (vec coll)
      (assoc index-a (nth coll index-b))
      (assoc index-b (nth coll index-a))))

(defn parse-int [str]
  (Integer/parseInt str))