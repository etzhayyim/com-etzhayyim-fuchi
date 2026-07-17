(ns fuchi.methods.test-displacement-scorecard
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.displacement-scorecard :as sc]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])))

(deftest test-live-refuse-matrix
  (let [legs (sc/live-refuse-matrix)]
    (is (seq legs))
    (is (every? #(false? (:admissible %)) legs))
    (is (every? #(= 0 (:cash-usd-micros %)) legs))))

#?(:clj
   (deftest test-build-scorecard
     (let [body (sc/build)]
       (is (false? (:scorecard/live body)))
       (is (= 0 (:scorecard/cash-usd-micros body)))
       (is (= [] (:scorecard/score-surface body)))
       (is (true? (:scorecard/all-live-refused body)))
       (is (pos? (:scorecard/admissible-cohorts body)))
       (is (pos? (:scorecard/refused-cohorts body)))
       (is (pos? (:scorecard/enrolled-subjects body)))
       (is (pos? (:scorecard/committed-usd-micros-yr body)))
       (is (pos? (:scorecard/booked-entries body)))
       (is (pos? (:scorecard/tenure-subjects body)))
       (is (pos? (:scorecard/tenure-admissible-cohorts body)))
       (is (= "L6" (or (ffirst (:scorecard/tenure-stage-counts body))
                       (some-> (:scorecard/tenure-stage-counts body) keys first))))
       (is (= pp/PRIORITY-STACK (:scorecard/priority-stack body)))
       (let [md (sc/scorecard-md body)]
         (is (str/includes? md "scorecard"))
         (is (str/includes? md "wellbecoming"))
         (is (str/includes? md "tenure"))
         (is (str/includes? md "all live legs refused"))
         (is (not (str/includes? md "| rank |")))))))

#?(:clj
   (deftest test-write-scorecard
     (let [paths (sc/write-scorecard!)]
       (is (.exists (io/file (:md paths))))
       (is (.exists (io/file (:edn paths))))
       (is (true? (:all-live-refused paths)))
       (is (false? (:live paths)))
       (is (pos? (:tenure-subjects paths)))
       (is (str/includes? (slurp (:md paths)) "No personal scores")))))
