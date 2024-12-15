(ns aoc-2024.day-09
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

(def real-data-smaller
  (parse-input "input-09-real-smaller.txt"))

(def test-data-simple
  (parse-input "input-09-test-simple.txt"))

(defn print-disk-map [disk-map]
  (doseq [{:keys [id type size]} disk-map]
    (let [chr (if (= type :free) "." id)]
      (print (string/join (repeat (or size 1) chr)))))
  (println))

(defn ->check-sum [disk-map]
  (->> disk-map
       (keep-indexed
         (fn [index {:keys [id type]}]
           (when (= type :data)
             (* index id))))
       (apply +)))

(defn ->disk-map [data]
  (flatten
    (map-indexed
      (fn [idx block]
        (let [type (if (= (mod idx 2) 1)
                     :free
                     :data)]
          (if (= type :free)
            (repeat block {:type type})
            (repeat block {:id (/ idx 2) :type type}))))
      data)))

(defn part-1 [data]
  (let [disk-map
        (->disk-map data)

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
                (swap left right disk-map')))))]

    (->check-sum disk-map-sorted)))

(defn part-2 [data]
  (let [disk-map
        (->disk-map data)

        disk-map-sorted
        (loop
          [left 0
           right (dec (count disk-map))
           disk-map' disk-map
           data-stack '()]
          (let [right-block (nth disk-map right)]
            (cond
              (>= left right)
              disk-map'

              (= (:type right-block) :data)
              (if (and
                    (seq data-stack)
                    (not= (:id right-block)
                          (:id (nth disk-map' (first data-stack)))))
                (let [maybe-free-stack
                      (loop
                        [lft left
                         free-stack '()]
                        (cond
                          (or
                            (>= lft (dec (count disk-map')))
                            (>= (count free-stack)
                                (count data-stack)))
                          (seq free-stack)

                          (= :free (:type (nth disk-map' lft)))
                          (recur
                            (inc lft)
                            (conj free-stack lft))

                          (= :data (:type (nth disk-map' lft)))
                          (recur
                            (inc lft)
                            '())))]
                  (if (and (>= (count maybe-free-stack)
                               (count data-stack))
                           ;; check that no index in maybe-free-stack is greater than in data-stack
                           (not (seq
                                  (flatten
                                    (for [f maybe-free-stack]
                                      (for [d data-stack
                                            :when (> f d)]
                                        true))))))
                    (let [swaps-to-do (map
                                        vector
                                        maybe-free-stack
                                        data-stack)
                          after-swaps (reduce
                                        (fn [dm [a b]]
                                          (swap a b dm))
                                        disk-map'
                                        swaps-to-do)]
                      (recur
                        left
                        (dec right)
                        after-swaps
                        (conj '() right)))
                    (recur
                      left
                      (dec right)
                      disk-map'
                      (conj '() right))))
                (recur
                  left
                  (dec right)
                  disk-map'
                  (conj data-stack right)))

              :else
              (recur
                left
                (dec right)
                disk-map'
                data-stack))))]

    (->check-sum disk-map-sorted)))

(defn day-9 []
  (prn (part-1 test-data))
  (prn (part-1 real-data))
  (prn (part-2 test-data))
  (prn (part-2 real-data)))

(day-9)