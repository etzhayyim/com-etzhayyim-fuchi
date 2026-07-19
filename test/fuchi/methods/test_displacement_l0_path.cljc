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
    (is (true? (get-in pkg [:couple :admissible])))
    (is (= :committed-offline-plan (get-in pkg [:couple :phase])))
    (is (false? (get-in pkg [:couple :commit-live-admissible])))
    (is (pos? (get-in pkg [:couple :committed-usd-micros-yr])))
    (is (pos? (get-in pkg [:couple :headroom-usd-micros-yr])))
    (let [s (first (:subjects pkg))]
      (is (true? (get-in s [:public-person :public-person?])))
      (is (= :dry-produce-plan (get-in s [:food-produce-plan :phase])))
      (is (false? (get-in s [:food-produce-plan :produce-executed])))
      (is (= :dry-produce-plan (get-in s [:care-produce-plan :phase])))
      (is (false? (get-in s [:care-produce-plan :care-delivery-executed])))
      (is (pos? (get-in s [:care-produce-plan :care-hours-floor-yr])))
      (is (= :dry-produce-plan (get-in s [:energy-produce-plan :phase])))
      (is (false? (get-in s [:energy-produce-plan :generate-executed])))
      (is (pos? (get-in s [:energy-produce-plan :kwh-floor-yr])))
      (is (= :dry-produce-plan (get-in s [:housing-produce-plan :phase])))
      (is (false? (get-in s [:housing-produce-plan :land-grant-executed])))
      (is (pos? (get-in s [:housing-produce-plan :housing-months-floor-yr])))
      (is (= :dry-produce-plan (get-in s [:tooling-produce-plan :phase])))
      (is (false? (get-in s [:tooling-produce-plan :fulfillment-executed])))
      (is (pos? (get-in s [:tooling-produce-plan :tool-units-floor-yr])))
      (is (= :dry-produce-plan (get-in s [:compute-produce-plan :phase])))
      (is (false? (get-in s [:compute-produce-plan :quota-executed])))
      (is (pos? (get-in s [:compute-produce-plan :gpu-hours-floor-yr])))
      (is (= "L4" (:stage s)))
      (is (= "L4" (get-in s [:ladder-fact :stage])))
      (is (= "L4" (get-in s [:stage-sustenance :stage])))
      (is (= "care" (first (get-in s [:stage-sustenance :rails]))))
      (is (= "housing" (second (get-in s [:stage-sustenance :rails]))))
      (is (some #{"housing"} (get-in s [:stage-sustenance :rails])))
      (is (some #{"tooling"} (get-in s [:stage-sustenance :rails])))
      (is (some #{"compute"} (get-in s [:stage-sustenance :rails])))
      (is (= :open (get-in s [:disclosure-hold :state])))
      (is (= :open (:disclosure-state s)))
      (is (false? (:disclosure-held? s)))
      (is (true? (:entitlements-may-flow? s)))
      (is (false? (get-in s [:disclosure-continuity :held?])))
      (is (= :R1-dry (get-in s [:food-package :phase])))
      (is (= :refused (get-in s [:food-gated-live-status :phase])))
      (is (false? (get-in s [:food-gated-live-status :admissible])))
      (is (false? (get-in s [:food-gated-live-status :produce-executed])))
      (is (= :R1-dry (get-in s [:energy-package :phase])))
      (is (= :refused (get-in s [:energy-gated-live-status :phase])))
      (is (false? (get-in s [:energy-gated-live-status :admissible])))
      (is (= :R1-dry (get-in s [:care-package :phase])))
      (is (= :refused (get-in s [:care-gated-live-status :phase])))
      (is (false? (get-in s [:care-gated-live-status :admissible])))
      (is (false? (get-in s [:care-gated-live-status :care-delivery-executed])))
      (is (= :R1-dry (get-in s [:housing-package :phase])))
      (is (= :refused (get-in s [:housing-gated-live-status :phase])))
      (is (false? (get-in s [:housing-gated-live-status :admissible])))
      (is (false? (get-in s [:housing-gated-live-status :land-grant-executed])))
      (is (false? (:land-grant-executed s)))
      (is (= :R1-dry (get-in s [:tooling-package :phase])))
      (is (= :refused (get-in s [:tooling-gated-live-status :phase])))
      (is (false? (get-in s [:tooling-gated-live-status :fulfillment-executed])))
      (is (= :R1-dry (get-in s [:compute-package :phase])))
      (is (= :refused (get-in s [:compute-gated-live-status :phase])))
      (is (false? (get-in s [:compute-gated-live-status :quota-executed])))
      (is (= :refused (get-in s [:r2-execute-status :phase])))
      (is (false? (get-in s [:r2-execute-status :executed])))
      (is (= :booked-offline (get-in s [:booking :phase])))
      (is (pos? (get-in s [:booking :entry-count])))
      (is (false? (get-in s [:booking :write-live-admissible])))
      (is (= 0 (get-in s [:booking :cash-usd-micros])))
      (is (contains? (get-in s [:booking :category-counts]) "care-flow"))
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
