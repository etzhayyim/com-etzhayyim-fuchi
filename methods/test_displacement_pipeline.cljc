(ns fuchi.methods.test-displacement-pipeline
  (:require [clojure.test :refer [deftest is]]
            #?(:clj [fuchi.methods.displacement-pipeline :as pipe])
            #?(:clj [clojure.java.io :as io])))

#?(:clj
   (deftest test-run-pipeline
     (let [out (pipe/run! :max-slots 1)]
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out)))
       (is (= [] (:score-surface out)))
       (is (true? (:all-live-refused out)))
       (is (pos? (:admissible-cohorts out)))
       (is (pos? (:tenure-subjects out)))
       (is (map? (:scorecard out)))
       (is (map? (:batch out))))))

#?(:clj
   (deftest test-write-all
     (let [out (pipe/write-all! :max-slots 1)]
       (is (.exists (io/file (get-in out [:paths :md]))))
       (is (false? (:deployed out)))
       (is (false? (:live out))))))
