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