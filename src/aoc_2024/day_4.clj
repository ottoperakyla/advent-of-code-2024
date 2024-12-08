(ns aoc-2024.day-4
  (:require [aoc-2024.utils :as utils]
            [clojure.string :as string]))

(def test-data
  [["M" "M" "M" "S" "X" "X" "M" "A" "S" "M"]
   ["M" "S" "A" "M" "X" "M" "S" "M" "S" "A"]
   ["A" "M" "X" "S" "X" "M" "A" "A" "M" "M"]
   ["M" "S" "A" "M" "A" "S" "M" "S" "M" "X"]
   ["X" "M" "A" "S" "A" "M" "X" "A" "M" "M"]
   ["X" "X" "A" "M" "M" "X" "X" "A" "M" "A"]
   ["S" "M" "S" "M" "S" "A" "S" "X" "S" "S"]
   ["S" "A" "X" "A" "M" "A" "S" "A" "A" "A"]
   ["M" "A" "M" "M" "M" "X" "M" "M" "M" "M"]
   ["M" "X" "M" "X" "A" "X" "M" "A" "S" "X"]])

(def real-data
  (->> (utils/read-file "input-04.txt")
       (string/split-lines)
       (mapv
         #(mapv str (vec %)))))

(defn part-1 [data]
  (let [word-to-find ["X" "M" "A" "S"]
        rows (count data)
        cols (count (first data))
        directions [[0 1]                                   ;;horizontal: left -> right
                    [0 -1]                                  ;;horizontal: right -> left

                    [1 0]                                   ;;vertical: top -> bottom
                    [-1 0]                                  ;;vertical: bottom -> top

                    [1 1]                                   ;;diagonal: top left -> bottom right
                    [-1 1]                                  ;;diagonal: bottom right -> top left
                    [-1 -1]                                 ;;diagonal: top right -> bottom left
                    [1 -1]                                  ;;diagonal: bottom left -> top right
                    ]]
    (reduce
      +
      (for [row (range rows)
            col (range cols)
            [row-delta col-delta] directions]
        (loop [current-row row
               current-col col
               word-index 0
               word-count 0]
          (cond
            (or
              (utils/out-of-bounds? data current-row current-col)
              (not= (get word-to-find word-index)
                    (get-in data [current-row current-col])))
            word-count

            :else
            (recur
              (+ current-row row-delta)
              (+ current-col col-delta)
              (inc word-index)
              (cond-> word-count
                      (= (inc word-index) (count word-to-find))
                      inc))))))))

(def x-masses #{"MSMS" "MSSM" "SMSM" "SMMS"})

(defn part-2 [data]
  (letfn [(row-col-indices [data]
            (let [rows (count data)
                  cols (count (first data))]
              (for [row (range rows)
                    col (range cols)]
                [row col])))

          (a-at-row-col? [[row col]]
            (= "A" (get-in data [row col])))

          (diagonals [row col]
            [[(dec row) (dec col)]                          ;; top left
             [(inc row) (inc col)]                          ;; bottom right
             [(inc row) (dec col)]                          ;; bottom left
             [(dec row) (inc col)]                          ;; top right
             ])

          (diagonal-out-of-bounds? [[row col]]
            (some
              (fn [[r c]]
                (utils/out-of-bounds? data r c))
              (diagonals row col)))

          (characters-at-diagonals [[row col]]
            (->> (diagonals row col)
                 (map (fn [[row col]] (get-in data [row col])))
                 (string/join)))]

    (->> data
         (row-col-indices)
         (filter a-at-row-col?)
         (remove diagonal-out-of-bounds?)
         (map characters-at-diagonals)
         (filter x-masses)
         (count))))

(defn day-4 []
  (prn (part-1 test-data))
  (prn (part-1 real-data))
  (prn (part-2 test-data))
  (prn (part-2 real-data)))

(day-4)