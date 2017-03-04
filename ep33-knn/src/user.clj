(ns user
  (:require [clojure.core.logic :as l]
            [clojure.core.unify :as u]
            [clojure.data.csv :as csv]
            [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.test :as t :refer [is testing are]]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as comb]
            [clojure.edn :as edn]))

(defn- ->num [x]
  (edn/read-string (str x "M")))

(defn- parse-data [data]
  {:post [(->> % (map (comp count :coords)) (reduce (fn [a b] (when (= a b) a))))]}
  (->> data
       (map #(array-map :tag (last %) :coords (mapv ->num (butlast %))))
       (remove (comp empty? :tag))))

(defn load-data [file]
  (shuffle (parse-data (csv/read-csv (io/reader file)))))

(defn distance-between [x y]
  (->> (map #(math/expt (- %1 %2) 2) x y)
       (reduce + 0)
       math/sqrt))

(defn guess-tag [coords k data]
  (->> data
       (map #(assoc % :distance (distance-between coords (:coords %))))
       (sort-by (comp min :distance))
       (take k)
       (group-by :tag)
       (sort-by (comp - count val))
       first
       key))

(defn train-classifier [k data]
  (fn classifier [coords]
    (guess-tag coords k data)))

(defn choose-samples [k data]
  (let [samples (partition-all k data)]
    (->> samples
         (map-indexed (fn [i sample]
                        (let [data (reduce concat (concat (take i samples) (drop (inc i) samples)))
                              classifier (train-classifier k data)]
                          {:sample sample :data data :classifier classifier}))))))

(defn test-classifier [k data]
  (/ (->> (choose-samples k data)
          (mapcat (fn [{:keys [sample data classifier]}]
                    (for [{:keys [coords tag]} sample
                          :let [guess (classifier coords)]]
                      (if (not= tag guess)
                        (do (prn sample tag guess) 0)
                        1))))
          (reduce +))
     (count data)))

(defonce iris-data (load-data "resources/iris.data"))
(defonce phishing-data (load-data "resources/phishing.data"))

(t/deftest test-something
  (testing "distance-between"
    (are [x y d] (= d (distance-between x y))
      [1 2 3] [1 2 3] 0
      [0 0] [3 4] 5))
  (testing "parse-data"
    (is (thrown? AssertionError (parse-data [["1" "2" "test"] ["1" "2" "3" "test2"]]))))
  (testing "guess-tag"
    (testing "some random data"
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
    (testing "leave one out"
      (let [data iris-data]
        (doseq [random-el data]
          (let [tag (guess-tag (:coords random-el) 3 (filter #{random-el} data))]
            (is (= tag (:tag random-el)))))))
    (testing "leave one out 2"
      (let [data phishing-data
            random-el (rand-nth data)
            tag (guess-tag (:coords random-el) 3 (filter #{random-el} data))]
        (is (= tag (:tag random-el)))))))
