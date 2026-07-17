(ns fuchi.methods.test-displacement-pipeline
  (:require [clojure.test :refer [deftest is]]
            #?(:clj [fuchi.methods.displacement-pipeline :as pipe])
            #?(:clj [clojure.java.io :as io])))

#?(:clj
   (deftest test-run-pipeline
     (let [out (pipe/run! :max-slots 1)
           sc (:scorecard out)]
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out)))
       (is (= [] (:score-surface out)))
       (is (true? (:all-live-refused out)))
       (is (pos? (:admissible-cohorts out)))
       (is (pos? (:tenure-subjects out)))
       (is (map? sc))
       (is (map? (:batch out)))
       (is (map? (:gov-route-counts out)))
       (is (pos? (get-in out [:gov-route-counts "council-lv7"] 0)))
       ;; L4 routes equal L4 enrolled subjects (not double-count tenure)
       (is (= (:scorecard/enrolled-subjects sc)
              (get-in out [:gov-route-counts "council-lv7"] 0)))
       (is (= (:scorecard/tenure-subjects sc)
              (get-in sc [:scorecard/tenure-gov-route-counts "council-lv7"] 0)))
       (is (pos? (:scorecard/gov-flowable-committed-usd-micros sc)))
       (is (pos? (:scorecard/gov-post-ratify-committed-usd-micros sc)))
       (is (< (:scorecard/gov-flowable-committed-usd-micros sc)
              (:scorecard/gov-post-ratify-committed-usd-micros sc)))
       (is (pos? (or (:scorecard/committed-post-ratify-usd-micros-yr sc) 0)))
       (is (pos? (:scorecard/tenure-gov-flowable-committed-usd-micros sc)))
       (is (pos? (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc)))
       (is (pos? (:scorecard/tenure-committed-usd-micros-yr sc)))
       (is (pos? (:scorecard/l4-disclosure-open sc)))
       (is (zero? (:scorecard/l4-disclosure-held sc))))))

#?(:clj
   (deftest test-write-all
     (let [out (pipe/write-all! :max-slots 1)]
       (is (.exists (io/file (get-in out [:paths :md]))))
       (is (false? (:deployed out)))
       (is (false? (:live out)))
       (is (map? (:audit out)))
       (is (.exists (io/file (get-in out [:audit :path]))))
       (is (true? (get-in out [:audit :event :audit/all-live-refused]))))))
