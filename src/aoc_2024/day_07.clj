(ns aoc-2024.day-07
  (:require [aoc-2024.utils :as utils]
            [clojure.string :as string]))

(defn parse-num [str]
  (bigint str))

(defn parse-input [filename]
  (->> (utils/read-file filename)
       (string/split-lines)
       (mapv (fn [row]
               (let [[sum equation] (string/split row #": ")]
                 [(parse-num sum)
                  (mapv #(parse-num %) (string/split equation #" "))])))))

(def test-data (parse-input "input-07-test.txt"))
(def real-data (parse-input "input-07-real.txt"))

(declare ||)

(defn calculate [equation]
  (loop [xs (seq equation)
         result (first xs)]
    (if (empty? (rest xs))
      result
      (let [op (second xs)
            next-num (nth xs 2)]
        (recur
          (drop 2 xs)
          (case op
            + (+ result next-num)
            * (* result next-num)
            || (bigint (str result next-num))))))))

(defn combinations
  ([ops n]
   (combinations ops n []))
  ([ops n res]
   (if (zero? n)
     [res]
     (apply concat
            (for [op ops]
              (combinations
                ops
                (dec n)
                (conj res op)))))))

(defn with-ops [equation ops]
  (butlast
    (interleave
      equation
      (conj ops (last ops)))))

(defn calculate-with-ops [ops]
  (fn [data]
    (->> data
         (filter
           (fn [[sum equation]]
             (some
               #(= sum %)
               (map
                 #(calculate (with-ops equation %))
                 (combinations ops (dec (count equation)))))))
         (map first)
         (apply +))))

(def part-1
  (calculate-with-ops ['+ '*]))

(def part-2
  (calculate-with-ops ['+ '* '||]))

(defn day-7 []
  #_(time (part-1 test-data))
  (time (part-1 real-data))
  #_(prn (part-2 test-data))
  (time (part-2 real-data)))

(day-7)