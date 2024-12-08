(ns aoc-2024.day-6
  (:require [clojure.string :as string]
            [aoc-2024.utils :refer [read-file
                                    out-of-bounds?
                                    mapv-indexed]]))

(defn parse-input [filename]
  (->> (read-file filename)
       (string/split-lines)
       (mapv
         #(mapv str (seq %)))))

(def test-data (parse-input "input-06-test.txt"))
(def test-data-part-2-1 (parse-input "input-06-part-2-test-1.txt"))
(def test-data-part-2-2 (parse-input "input-06-part-2-test-2.txt"))
(def real-data (parse-input "input-06-real.txt"))

(def guard-characters
  #{"^" "<" ">" "v"})

(defn ->starting-pos [grid]
  (let [rows (range (count grid))
        cols (range (count (first grid)))]
    (first
      (for [row rows
            col cols
            :let [current-char (get-in grid [row col])]
            :when (guard-characters current-char)]
        [current-char row col]))))

(defn ->next-pos [pos grid]
  (let [[dir row col] pos
        [next-row next-col] (case dir
                              "^"
                              [(dec row) col]

                              "<"
                              [row (dec col)]

                              ">"
                              [row (inc col)]

                              "v"
                              [(inc row) col])]
    (cond
      (out-of-bounds? grid next-row next-col)
      :guard-exited

      (= "#" (get-in grid [next-row next-col]))
      (case dir
        "^"
        (->next-pos [">" row col] grid)

        "<"
        (->next-pos ["^" row col] grid)

        ">"
        (->next-pos ["v" row col] grid)

        "v"
        (->next-pos ["<" row col] grid))

      :else
      [dir next-row next-col])))

(defn show-guard-pos [[guard-char guard-row guard-col] grid]
  (doseq [current-row (range (count grid))]
    (doseq [current-col (range (count (first grid)))]
      (cond
        (and (= guard-row current-row)
             (= guard-col current-col))
        (print guard-char)

        (guard-characters (get-in grid [current-row current-col]))
        (print ".")

        :else
        (print (get-in grid [current-row current-col]))))
    (println))

  ;; clear screen
  (print (str (char 27) "[2J"))
  (Thread/sleep 1000))

(def show-path? false)

(defn ->visited-positions [grid]
  (let [starting-pos (->starting-pos grid)]
    (loop [guard-pos starting-pos
           visited-positions #{(rest guard-pos)}]
      (when show-path?
        (show-guard-pos guard-pos grid))
      (let [next-pos (->next-pos guard-pos grid)]
        (if (= :guard-exited next-pos)
          (conj visited-positions (rest guard-pos))
          (recur
            (->next-pos guard-pos grid)
            (conj visited-positions (rest guard-pos))))))))

(defn part-1 [grid]
  (count (->visited-positions grid)))

(defn contains-loop? [grid]
  (let [starting-pos (->starting-pos grid)]
    (loop [guard-pos starting-pos
           visited #{guard-pos}]
      (when show-path?
        (show-guard-pos guard-pos grid))
      (let [next-pos (->next-pos guard-pos grid)]
        (cond
          (visited next-pos)
          true

          (= :guard-exited next-pos)
          false

          :else
          (recur
            (->next-pos guard-pos grid)
            (conj visited guard-pos)))))))

(defn place-obstruction-at [grid]
  (fn [[target-row target-col]]
    (mapv-indexed
      (fn [current-row row]
        (mapv-indexed
          (fn [current-col col]
            (if (and (= target-row current-row)
                     (= target-col current-col))
              "#"
              col))
          row))
      grid)))

(defn part-2 [grid]
  (let [visited-positions
        (->visited-positions grid)

        empty-cells-within-visited-positions
        (for [row (range (count grid))
              col (range (count (first grid)))
              :let [current-char (get-in grid [row col])]
              :when (and (= "." current-char)
                         (visited-positions [row col]))]
          [row col])]

    (->> empty-cells-within-visited-positions
         (filter (comp contains-loop? (place-obstruction-at grid)))
         (count))))

(defn day-6 []
  (prn (part-1 real-data))
  (prn (part-2 real-data)))

(day-6)