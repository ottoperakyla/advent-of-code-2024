(ns aoc-2024.day-3
  (:require [aoc-2024.utils :as utils]
            [clojure.string :as string]))

(def test-data-1
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
;   ^                           ^                       ^        ^

(def test-data-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
;+  ^                  -                                      +    ^

(def real-data
  (utils/read-file "input-03.txt"))

(defn part-1 [data]
  (->> data
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map rest)
       (map
         #(*
            (Integer/parseInt (first %))
            (Integer/parseInt (second %))))
       (reduce +)))

(defn part-2-loop [data]
  (let [instructions (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)" data)]
    (loop [xs (seq instructions)
           calculation-enabled? true
           sum 0]
      (if xs
        (let [instruction (first xs)
              function-call (first instruction)
              arguments (rest instruction)
              instruction-type (if
                                 (string/starts-with? function-call "mul")
                                 :calculation
                                 :set-calculation-enabled)

              set-calculation-enabled-to
              (case instruction-type
                :set-calculation-enabled
                (= function-call "do()")

                :calculation
                calculation-enabled?)]
          (recur
            (next xs)
            set-calculation-enabled-to
            (cond-> sum
                    (and calculation-enabled?
                         (= instruction-type :calculation))
                    (+
                      (*
                        (Integer/parseInt (first arguments))
                        (Integer/parseInt (second arguments)))))))
        sum))))

(defn part-2-reduce [data]
  (->> data
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)")
       (reduce
         (fn [{prev-calculation-enabled? :calculation-enabled?
               :keys                     [sum]}
              [function-call argument-1 argument-2]]
           (let [do? (string/starts-with? function-call "do")
                 do-not? (string/starts-with? function-call "don't")
                 mul? (string/starts-with? function-call "mul")
                 calculation-enabled? (cond
                                        do-not? false
                                        do? true
                                        mul? prev-calculation-enabled?)]
             {:calculation-enabled? calculation-enabled?
              :sum                  (cond-> sum
                                            (and calculation-enabled? mul?)
                                            (+ (*
                                                 (Integer/parseInt argument-1)
                                                 (Integer/parseInt argument-2))))}))
         {:calculation-enabled? true
          :sum                  0})))

(defn day-3 []
  (prn (part-1 real-data))
  (prn (part-2-loop real-data))
  (prn (part-2-reduce real-data)))

(day-3)