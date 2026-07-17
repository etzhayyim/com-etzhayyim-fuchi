(ns fuchi.methods.test-displacement-l0-path
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.displacement-l0-path :as d]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

(deftest test-funded-event-enrolls
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "cohort-robotics-remote-2026"
             :displaced-count 48
             :surplus-usd-micros-yr 120000000000
             :funded true})
        pkg (d/run-for-event ev :max-slots 2)]
    (is (= :offline-enrolled (:phase pkg)))
    (is (= 2 (count (:subjects pkg))))
    (is (false? (:live pkg)))
    (is (= 0 (:cash-usd-micros pkg)))
    (is (= [] (:score-surface pkg)))
    (let [s (first (:subjects pkg))]
      (is (true? (get-in s [:public-person :public-person?])))
      (is (= :dry-produce-plan (get-in s [:food-produce-plan :phase])))
      (is (false? (get-in s [:food-produce-plan :produce-executed])))
      (is (= :dry-produce-plan (get-in s [:care-produce-plan :phase])))
      (is (false? (get-in s [:care-produce-plan :care-delivery-executed])))
      (is (pos? (get-in s [:care-produce-plan :care-hours-floor-yr])))
      (pp/assert-no-public-scores! (:public-person s)))))

(deftest test-unfunded-refused
  (let [ev (couple/make-displacement-event
            {:displacing-actor "hataori"
             :cohort-id "cohort-hataori-2026"
             :displaced-count 30
             :surplus-usd-micros-yr 0
             :funded false})
        pkg (d/run-for-event ev)]
    (is (= :refused (:phase pkg)))
    (is (empty? (:subjects pkg)))
    (is (false? (:live pkg)))
    (is (= 0 (:cash-usd-micros pkg)))))

#?(:clj
   (deftest test-seed-batch
     (let [out (d/run-default-seed :max-slots 2)]
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out)))
       (is (pos? (:admissible-cohorts out)))
       (is (pos? (:refused-cohorts out)))
       (is (pos? (:enrolled-subjects out)))
       (is (= [] (:score-surface out))))))
