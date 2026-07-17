(ns fuchi.methods.test-displacement-pipeline
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
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
       (is (zero? (:scorecard/l4-disclosure-held sc)))
       ;; top-level package facts (parity with scorecard)
       (is (pos? (:gov-flowable-committed-usd-micros out)))
       (is (pos? (:gov-post-ratify-committed-usd-micros out)))
       (is (>= (:gov-post-ratify-committed-usd-micros out)
               (:gov-flowable-committed-usd-micros out)))
       (is (zero? (:housing-land-grant-executed out)))
       (is (pos? (:housing-council-held out)))
       (is (pos? (:r2-status-count out)))
       (is (pos? (:r2-refused out)))
       (is (zero? (:r2-executed out)))
       (is (true? (:all-r2-not-executed out))))))

#?(:clj
   (deftest test-write-all
     (let [out (pipe/write-all! :max-slots 1 :include-public false)]
       (is (.exists (io/file (get-in out [:paths :md]))))
       (is (false? (:deployed out)))
       (is (false? (:live out)))
       (is (false? (:package-ready out)))
       (is (nil? (:public-package out)))
       (is (map? (:audit out)))
       (is (.exists (io/file (get-in out [:audit :path]))))
       (is (true? (get-in out [:audit :event :audit/all-live-refused])))
       (is (zero? (get-in out [:audit :event :audit/housing-land-grant-executed] 0)))
       (is (pos? (get-in out [:audit :event :audit/gov-post-ratify-committed-usd-micros] 0))))))

#?(:clj
   (deftest test-write-all-with-public-package
     (let [out (pipe/write-all! :max-slots 1 :include-public true)
           pkg (:public-package out)
           st (:deploy-status out)]
       (is (false? (:deployed out)))
       (is (false? (:wrangler-invoked out)))
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out)))
       (is (true? (:package-ready out)))
       (is (map? pkg))
       (is (false? (:deployed pkg)))
       (is (= :refused (:phase st)))
       (is (false? (:authorized-to-deploy st)))
       (is (zero? (:housing-land-grant-executed out)))
       (is (pos? (:gov-post-ratify-committed-usd-micros out)))
       (is (.exists (io/file (:index pkg))))
       (is (.exists (io/file (:wrangler pkg))))
       (when-let [rb (:deploy-runbook pkg)]
         (is (.exists (io/file rb))))
       (let [html (slurp (:index pkg))]
         (is (str/includes? html "public surface"))
         (is (str/includes? html "plan-only"))
         (is (str/includes? html "Pipeline audit summary"))
         (is (not (re-find #"(?i)priority-rank" html)))))))
