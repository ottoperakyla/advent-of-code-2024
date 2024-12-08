(ns aoc-2024.day-5
  (:require [aoc-2024.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn parse-input [filename]
  (let [data
        (map string/split-lines
             (-> (utils/read-file filename)
                 (string/split #"\n\n")))

        rules
        (->> data
             (first)
             (map #(string/split % #"\|"))
             (map
               (fn [xs]
                 (map #(Integer/parseInt %) xs))))

        updates
        (->> data
             (second)
             (map #(string/split % #","))
             (map
               (fn [xs]
                 (map #(Integer/parseInt %) xs))))]
    [rules updates]))

(def test-data
  (parse-input "input-05-test.txt"))

(def real-data
  (parse-input "input-05-real.txt"))

(defn get-depended->dependency [rules]
  (reduce
    (fn [depended->dependencies [dependency depended]]
      (update depended->dependencies depended conj dependency))
    {}
    rules))

(defn get-depended->dependency-for-this-update [depended->dependency update']
  (->> depended->dependency
       (keys)
       (filter (set update'))
       (map (fn [depended]
              [depended
               (filter
                 (set update')
                 (get depended->dependency depended))]))
       (into {})))

(defn pages-in-wrong-order? [depended->dependency]
  (fn [update']
    (let [depended->dependency-for-this-update
          (get-depended->dependency-for-this-update depended->dependency update')]
      (loop
        [update'' (seq update')
         seen #{}]
        (when update''
          (let [page
                (first update'')

                page-dependencies
                (set (get depended->dependency-for-this-update page))

                missing-dependencies
                (set/difference
                  page-dependencies
                  seen)]
            (if (empty? missing-dependencies)
              (recur
                (next update'')
                (conj seen page))
              true)))))))

(defn part-1 [data]
  (let [[rules updates] data
        depended->dependency (get-depended->dependency rules)]
    (->> updates
         (remove (pages-in-wrong-order? depended->dependency))
         (map utils/get-middle-element)
         (apply +))))

(defn part-1-2 [data]
  (->> (second data)
       (map
         (fn [update']
           (prn "validate update" update')
           (let [rules
                 (let [r (set update')]
                   (loop [xs (seq (first data))
                          graph {}]
                     (if xs
                       (let [[before after] (first xs)]
                         (if (and (r before) (r after))
                           (recur
                             (next xs)
                             (update graph after conj before))
                           (recur
                             (next xs)
                             graph)))
                       graph)))]
             (reduce
               (fn [acc page]
                 (prn "before this page:" (get rules page))
                 {:seen   (conj (:seen acc) page)
                  :valid? (and
                            (:valid? acc)
                            (every?
                              (fn [r]
                                (contains? (:seen acc) r))
                              (get rules page)
                              ))})
               {:seen   #{}
                :valid? true}
               update'))))
       (filter :valid?)
       (count)))

(defn fix-page-ordering
  ([depended->dependency update']
   (fix-page-ordering depended->dependency
                      update'
                      (atom #{})
                      (atom [])))
  ([depended->dependency update' seen sorted]
   (->> update'
        (map
          (fn [page]
            (when-not (contains? @seen page)
              (swap! seen conj page)
              (fix-page-ordering
                depended->dependency
                (get depended->dependency page)
                seen
                sorted)
              (swap! sorted conj page))))
        (doall))
   @sorted))

(defn part-2 [data]
  (let [[rules updates] data
        depended->dependency (get-depended->dependency rules)]
    (->> updates
         (filter (pages-in-wrong-order? depended->dependency))
         (map
           #(fix-page-ordering
              (get-depended->dependency-for-this-update depended->dependency %)
              %))
         (map utils/get-middle-element)
         (apply +))))

(defn day-5 []
  #_(prn (part-1 test-data))
  (prn (part-1 real-data))
  #_(prn (part-2 test-data))
  (prn (part-2 real-data)))

(day-5)




