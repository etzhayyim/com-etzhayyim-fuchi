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
              :liquidity-imputed-usd-micros-yr 1500000000})]
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))
    (is (= [] (:score-surface out)))
    (is (true? (get-in out [:public-person :public-person?])))
    (is (= :dry-produce-plan (get-in out [:food-produce-plan :phase])))
    (is (false? (get-in out [:food-produce-plan :produce-executed])))
    (is (= :dry-produce-plan (get-in out [:energy-produce-plan :phase])))
    (is (false? (get-in out [:energy-produce-plan :generate-executed])))
    (is (pos? (get-in out [:energy-produce-plan :kwh-floor-yr])))
    (is (= :dry-ack (get-in out [:energy-receive :phase])))
    (is (false? (get-in out [:energy-receive :generate-invoked])))
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
    (pp/assert-no-public-scores! (:public-person out))))

#?(:clj
   (deftest test-path-with-itonami
     (let [out (path/run-with-itonami-seed
                {:subject-did "did:web:etzhayyim.com:member:path-demo2"
                 :food-imputed-usd-micros-yr 1000000})]
       (is (seq (:itonami-displacement out)))
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out))))))
