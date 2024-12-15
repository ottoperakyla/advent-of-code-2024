(ns aoc-2024.day-08
  (:require [aoc-2024.utils :as utils :refer [mapv-indexed]]))

(def test-data-simple-1
  (utils/parse-grid "input-08-simple-1.txt"))

(def test-data-simple-2
  (utils/parse-grid "input-08-simple-2.txt"))

(def test-data-two-freqs
  (utils/parse-grid "input-08-two-freqs.txt"))

(def test-data
  (utils/parse-grid "input-08-test.txt"))

(def real-data
  (utils/parse-grid "input-08-real.txt"))

(def test-data-part-2
  (utils/parse-grid "input-08-part-2-test.txt"))

(defn empty-space? [char]
  (= "." char))

(def antenna?
  (comp not empty-space?))

(defn with-antinodes [antinodes data]
  (mapv-indexed
    (fn [row-index row]
      (mapv-indexed
        (fn [col-index col]
          (if (antinodes [row-index col-index])
            "#"
            col))
        row))
    data))

(defn ->antennas [data]
  (for [row (range (count data))
        col (range (count (first data)))
        :when (antenna? (get-in data [row col]))]
    [row col]))

(defn part-1 [data]
  (let [antennas
        (->antennas data)

        antinodes
        (->>
          (for [[row col] antennas]
            (for [[row' col'] antennas
                  :let [row-delta (- row row')
                        col-delta (- col col')
                        antinode-row (+ row row-delta)
                        antinode-col (+ col col-delta)]
                  :when (and
                          ;; check that antennas are the same frequency
                          (= (get-in data [row col])
                             (get-in data [row' col']))

                          ;; dont check the antenna against itself
                          (not (and
                                 (= row row')
                                 (= col col')))

                          ;; remove out of bounds coordinates
                          (not (utils/out-of-bounds?
                                 data
                                 antinode-row
                                 antinode-col)))]
              [antinode-row antinode-col]))
          (flatten)
          (partition 2)
          (set))]
    (utils/print-grid
      (with-antinodes antinodes data))
    (count antinodes)))

(defn part-2 [data]
  (let [antennas
        (->antennas data)

        antinodes
        (->>
          (mapcat
            (fn [[row col]]
              (for [[row' col'] antennas
                    :let [row-delta (- row' row)
                          col-delta (- col' col)]
                    :when (and
                            ;; check that antennas are the same frequency
                            (= (get-in data [row col])
                               (get-in data [row' col']))

                            ;; dont check the antenna against itself
                            (not (and
                                   (= row row')
                                   (= col col'))))]
                (loop
                  [current-row row
                   current-col col
                   antinodes [[current-row current-col]]]
                  (let [next-row (+ current-row row-delta)
                        next-col (+ current-col col-delta)]
                    (if (not (utils/out-of-bounds?
                               data
                               next-row
                               next-col))
                      (recur
                        next-row
                        next-col
                        (conj antinodes [next-row next-col]))
                      antinodes)))))
            antennas)
          (flatten)
          (partition 2)
          (set))]
    (utils/print-grid
      (with-antinodes antinodes data))
    (count antinodes)))

(defn day-8 []
  (prn (part-1 real-data))
  (prn (part-2 real-data)))

(day-8)