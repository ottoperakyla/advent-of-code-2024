(ns aoc-2024.day-9
  (:require [aoc-2024.utils :as utils]
            [clojure.string :as string]))

(defn parse-input [input]
  (->> input
       (utils/read-file)
       (seq)
       (map #(Integer/parseInt (str %)))))

(def test-data
  (parse-input "input-09-test.txt"))

(defn print-disk-map [disk-map]
  (doseq [{:keys [id type size]} disk-map]
    (let [chr (if (= type :free-space) "." id)]
      (print (string/join (repeat size chr)))))
  (println))

(defn part-1 [data]
  (print-disk-map
    (loop [idx 0
           id 0
           disk-map []
           blocks (seq data)]
      (if blocks
        (let [block (first blocks)
              type (if (= (mod idx 2) 1)
                     :free-space
                     :data)]
          (recur
            (inc idx)
            (if (= type :free-space)
              (inc id)
              id)
            (conj disk-map {:id id :type type :size block})
            (next blocks)))
        disk-map))))

(defn part-2 [data]
  data)

(defn day-9 []
  (prn (part-1 test-data)))

(day-9)