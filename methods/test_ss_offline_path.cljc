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
              :care-imputed-usd-micros-yr 1000000000})]
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))
    (is (= [] (:score-surface out)))
    (is (true? (get-in out [:public-person :public-person?])))
    (is (= :dry-produce-plan (get-in out [:food-produce-plan :phase])))
    (is (false? (get-in out [:food-produce-plan :produce-executed])))
    (is (= :dry-ack (get-in out [:energy-receive :phase])))
    (is (false? (get-in out [:energy-receive :generate-invoked])))
    (is (= :R1-dry (get-in out [:care-package :phase])))
    (is (= "care-iyashi" (get-in out [:care-package :rail-kind])))
    (pp/assert-no-public-scores! (:public-person out))))

#?(:clj
   (deftest test-path-with-itonami
     (let [out (path/run-with-itonami-seed
                {:subject-did "did:web:etzhayyim.com:member:path-demo2"
                 :food-imputed-usd-micros-yr 1000000})]
       (is (seq (:itonami-displacement out)))
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out))))))
