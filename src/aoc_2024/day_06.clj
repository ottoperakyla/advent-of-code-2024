(ns aoc-2024.day-06
  (:require [aoc-2024.utils :refer [out-of-bounds?
                                    parse-grid]]))

(def test-data (parse-grid "input-06-test.txt"))
(def test-data-part-2-1 (parse-grid "input-06-part-2-test-1.txt"))
(def test-data-part-2-2 (parse-grid "input-06-part-2-test-2.txt"))
(def real-data (parse-grid "input-06-real.txt"))

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

(defn contains-loop? [starting-pos]
  (fn [grid]
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
    (assoc-in grid [target-row target-col] "#")))

(defn part-2 [grid]
  (let [visited-positions
        (->visited-positions grid)

        empty-cells-within-visited-positions
        (filterv
          (fn [[row col]]
            (= "." (get-in grid [row col])))
          visited-positions)

        starting-pos
        (->starting-pos grid)]

    (->> empty-cells-within-visited-positions
         (pmap
           (comp
             (contains-loop? starting-pos)
             (place-obstruction-at grid)))
         (doall)
         (filterv identity)
         (count))))

(defn day-6 []
  (time (part-1 real-data))
  (time (part-2 real-data)))

(day-6)