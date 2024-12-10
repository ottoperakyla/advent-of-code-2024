(ns aoc-2024.day-9
  (:require [aoc-2024.utils :as utils :refer [swap]]
            [clojure.string :as string]
            [clojure.pprint :as pprint]))

(defn parse-input [input]
  (->> input
       (utils/read-file)
       (seq)
       (map #(Integer/parseInt (str %)))))

(def test-data
  (parse-input "input-09-test.txt"))

(def real-data
  (parse-input "input-09-real.txt"))

(def test-data-simple
  (parse-input "input-09-test-simple.txt"))

(defn print-disk-map [disk-map]
  (doseq [{:keys [id type]} disk-map]
    (let [chr (if (= type :free) "." id)]
      (print chr)))
  (println))

(defn part-1 [data]
  (let [disk-map (loop [idx 0
                        id 0
                        disk-map []
                        blocks (seq data)]
                   (if blocks
                     (let [block (first blocks)
                           type (if (= (mod idx 2) 1)
                                  :free
                                  :data)]
                       (recur
                         (inc idx)
                         (if (= type :free)
                           (inc id)
                           id)
                         (if (= type :free)
                           (concat disk-map
                                   (repeat block {:type type}))
                           (concat disk-map
                                   (repeat block {:id id :type type})))
                         (next blocks)))
                     disk-map))

        disk-map-sorted
        (loop
          [left 0
           right (dec (count disk-map))
           disk-map' disk-map]
          (let [{type-left :type} (nth disk-map' left)
                {type-right :type} (nth disk-map' right)]
            (cond
              (>= left right)
              disk-map'

              (and (= type-left :data)
                   (= type-right :data))
              (recur
                (inc left)
                right
                disk-map')

              (and (= type-left :free)
                   (= type-right :free))
              (recur
                left
                (dec right)
                disk-map')

              (and (= type-left :data)
                   (= type-right :free))
              (recur
                (inc left)
                (dec right)
                disk-map')

              :else
              (recur
                (inc left)
                (dec right)
                (swap left right disk-map')))))


        check-sum
        (->> disk-map-sorted
             (keep-indexed
               (fn [index {:keys [id type]}]
                 (when (= type :data)
                   (* index id))))
             (apply +))]

    #_(pprint/pprint disk-map)
    #_(print-disk-map disk-map)

    check-sum))

(defn part-2 [data]
  data)

(defn day-9 []
  (prn (part-1 test-data))
  #_(prn (part-1 real-data))
  )

(day-9)