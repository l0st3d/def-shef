(ns user
  (:require [clojure.core.logic :as l]
            [clojure.core.unify :as u]
            [clojure.data.csv :as csv]
            [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.test :as t :refer [is testing are]]
            [clojure.math.numeric-tower :as math]
            [clojure.edn :as edn]))

(defn- ->num [x]
  (edn/read-string (str x "M")))

(t/with-test
  (defn- parse-data [data]
    {:post [(->> % (map :coords) (every? (comp (partial = (count (:coords (first %)))) count)))]}
    (->> data
         (map #(array-map :tag (last %) :coords (mapv ->num (butlast %))))
         (remove (comp empty? :tag))))

  (is (thrown? AssertionError (parse-data [["1" "2" "test"] ["1" "2" "3" "test2"]]))))

(defn load-data [file]
  (parse-data (csv/read-csv (io/reader file))))

(t/with-test
  (defn distance-between [x y]
    (->> (map #(math/expt (- %1 %2) 2) x y)
         (reduce + 0)
         math/sqrt))

  (are [x y d] (= d (distance-between x y))
    [1 2 3] [1 2 3] 0
    [0 0] [3 4] 5))

(t/with-test
  (defn guess-tag [coords k data]
    (->> data
         (map #(assoc % :distance (distance-between coords (:coords %))))
         (sort-by (comp min :distance))
         (take k)
         (group-by :tag)
         (sort-by (comp count val))
         last
         key))

  (are [t c k d] (= t (guess-tag c k d))
    "test" [1] 3 [{:coords [0] :tag "test"}
                  {:coords [2] :tag "test"}
                  {:coords [5] :tag "fail"}
                  {:coords [10] :tag "fail"}
                  {:coords [20] :tag "fail"}]
    "test" [1] 5 [{:coords [0] :tag "fail"}
                  {:coords [2] :tag "fail"}
                  {:coords [5] :tag "test"}
                  {:coords [10] :tag "test"}
                  {:coords [20] :tag "test"}]))

(comment
  ;; leave one out
  (let [data      (load-data "resources/iris.data")
        random-el (rand-nth data)
        data      (filter #{random-el} data)
        tag       (guess-tag (:coords random-el) 3 data)]
    (= tag (:tag random-el))))
