(ns fuchi.methods.test-ss-offline-path
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.ss-offline-path :as path]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])))

(deftest test-food-energy-offline-path
  (let [out (path/run-food-path
             {:subject-did "did:web:etzhayyim.com:member:path-demo"
              :food-imputed-usd-micros-yr 2000000000
              :energy-imputed-usd-micros-yr 1500000000
              :care-imputed-usd-micros-yr 1000000000
              :housing-imputed-usd-micros-yr 12000000000
              :tooling-imputed-usd-micros-yr 500000000
              :compute-imputed-usd-micros-yr 800000000
              :liquidity-imputed-usd-micros-yr 1500000000})
        sum (:priority-path-summary out)]
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))
    (is (= [] (:score-surface out)))
    (is (true? (get-in out [:public-person :public-person?])))
    ;; (1) L0 enroll offline
    (is (= "L0" (:l0-stage sum)))
    (is (false? (:l0-published sum)))
    (is (some? (:l0-token-stub sum)))
    (is (some? (get-in out [:l0 :vow :token-id])))
    ;; (2) disclosure continuity open on fresh path
    (is (= :open (get-in out [:disclosure-hold :state])))
    (is (true? (:entitlements-may-flow? sum)))
    (is (map? (:disclosure-continuity out)))
    (is (map? (:disclosure-continuity-series out)))
    (is (= :open (get-in out [:disclosure-continuity-series :final-state])))
    ;; held stress freezes food R1 while open path stays dry
    (is (true? (get-in out [:disclosure-held-stress :held?])))
    (is (= "refused" (:held-stress-food-phase sum)))
    ;; (3) mitsuho/hikari R1 → gated-live DESIGN default refuse
    (is (= :R1-dry (get-in out [:food-package :phase])))
    (is (false? (get-in out [:food-gated-live-status :admissible])))
    (is (false? (get-in out [:food-gated-live-status :produce-executed])))
    (is (= :R1-dry (get-in out [:energy-package :phase])))
    (is (false? (get-in out [:energy-gated-live-status :admissible])))
    (is (false? (get-in out [:energy-gated-live-status :generate-executed] false)))
    (is (= :dry-produce-plan (get-in out [:food-produce-plan :phase])))
    (is (false? (get-in out [:food-produce-plan :produce-executed])))
    (is (= :dry-ack (get-in out [:food-receive :phase])))
    (is (= :dry-produce-plan (get-in out [:energy-produce-plan :phase])))
    (is (false? (get-in out [:energy-produce-plan :generate-executed])))
    (is (pos? (get-in out [:energy-produce-plan :kwh-floor-yr])))
    (is (= :dry-ack (get-in out [:energy-receive :phase])))
    (is (false? (get-in out [:energy-receive :generate-invoked])))
    ;; R2 execute membrane refuse
    (is (= :refused (get-in out [:food-r2-execute-status :phase])))
    (is (false? (get-in out [:food-r2-execute-status :executed])))
    (is (= :refused (get-in out [:energy-r2-execute-status :phase])))
    (is (false? (get-in out [:energy-r2-execute-status :executed])))
    (is (false? (:mitsuho-gated-admissible sum)))
    (is (false? (:hikari-gated-admissible sum)))
    (is (false? (:r2-food-executed sum)))
    (is (false? (:r2-energy-executed sum)))
    (is (= :R1-dry (get-in out [:care-package :phase])))
    (is (= "care-iyashi" (get-in out [:care-package :rail-kind])))
    (is (= :dry-produce-plan (get-in out [:care-produce-plan :phase])))
    (is (false? (get-in out [:care-produce-plan :care-delivery-executed])))
    (is (pos? (get-in out [:care-produce-plan :care-hours-floor-yr])))
    (is (= :dry-ack (get-in out [:care-receive :phase])))
    (is (false? (get-in out [:care-receive :care-delivery-invoked])))
    (is (= :R1-dry (get-in out [:housing-package :phase])))
    (is (= "housing-commons" (get-in out [:housing-package :rail-kind])))
    (is (= "commons-land" (get-in out [:housing-package :provider-did])))
    (is (= :dry-produce-plan (get-in out [:housing-produce-plan :phase])))
    (is (false? (get-in out [:housing-produce-plan :land-grant-executed])))
    (is (pos? (get-in out [:housing-produce-plan :housing-months-floor-yr])))
    (is (= :dry-ack (get-in out [:housing-receive :phase])))
    (is (false? (get-in out [:housing-receive :land-grant-invoked])))
    (is (= :R1-dry (get-in out [:tooling-package :phase])))
    (is (= "tooling-okaimono" (get-in out [:tooling-package :rail-kind])))
    (is (= :dry-produce-plan (get-in out [:tooling-produce-plan :phase])))
    (is (false? (get-in out [:tooling-produce-plan :fulfillment-executed])))
    (is (pos? (get-in out [:tooling-produce-plan :tool-units-floor-yr])))
    (is (= :dry-ack (get-in out [:tooling-receive :phase])))
    (is (false? (get-in out [:tooling-receive :fulfillment-invoked])))
    (is (= :R1-dry (get-in out [:compute-package :phase])))
    (is (= "compute-murakumo" (get-in out [:compute-package :rail-kind])))
    (is (= "murakumo" (get-in out [:compute-package :provider-did])))
    (is (= :dry-produce-plan (get-in out [:compute-produce-plan :phase])))
    (is (false? (get-in out [:compute-produce-plan :quota-executed])))
    (is (pos? (get-in out [:compute-produce-plan :gpu-hours-floor-yr])))
    (is (= :dry-ack (get-in out [:compute-receive :phase])))
    (is (false? (get-in out [:compute-receive :quota-invoked])))
    (is (= :R1-dry (get-in out [:liquidity-package :phase])))
    (is (= "liquidity-warifu" (get-in out [:liquidity-package :rail-kind])))
    (is (true? (get-in out [:liquidity-package :member-principal])))
    (is (= 0 (get-in out [:liquidity-package :cash-usd-micros])))
    (is (= :dry-ack (get-in out [:liquidity-receive :phase])))
    (is (false? (get-in out [:liquidity-receive :loan-invoked])))
    (is (true? (get-in out [:liquidity-receive :member-principal])))
    (pp/assert-no-public-scores! (:public-person out))
    (pp/assert-no-public-scores! sum)))

#?(:clj
   (deftest test-path-with-itonami
     (let [out (path/run-with-itonami-seed
                {:subject-did "did:web:etzhayyim.com:member:path-demo2"
                 :food-imputed-usd-micros-yr 1000000})]
       (is (seq (:itonami-displacement out)))
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out))))))
