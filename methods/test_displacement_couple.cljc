(ns fuchi.methods.test-displacement-couple
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.displacement-couple :as dc]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.public-person :as pp]))

(deftest test-covered-within-earmark
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "c1"
             :displaced-count 10
             :surplus-usd-micros-yr 120000000000
             :funded true})
        ear (couple/earmark-from-surplus ev)
        subjects [{:booking {:in-kind-total-usd-micros 10000000000}}
                  {:booking {:in-kind-total-usd-micros 5000000000}}]
        out (dc/commit-offline-plan ev ear subjects)]
    (is (= :committed-offline-plan (:phase out)))
    (is (true? (:admissible out)))
    (is (true? (:committed out)))
    (is (false? (:commit-live out)))
    (is (false? (:commit-live-admissible out)))
    (is (= 15000000000 (:committed-usd-micros-yr out)))
    (is (pos? (:headroom-usd-micros-yr out)))
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))
    (pp/assert-no-public-scores! (dc/public-couple-summary out))))

(deftest test-over-earmark-refused
  (let [ev (couple/make-displacement-event
            {:displacing-actor "x"
             :cohort-id "c2"
             :displaced-count 2
             :surplus-usd-micros-yr 1000000
             :funded true})
        ear (couple/earmark-from-surplus ev)
        subjects [{:booking {:in-kind-total-usd-micros 999999999}}]
        out (dc/commit-offline-plan ev ear subjects)]
    (is (= :refused (:phase out)))
    (is (false? (:admissible out)))
    (is (false? (:committed out)))))

(deftest test-unfunded-refused
  (let [ev (couple/make-displacement-event
            {:displacing-actor "h"
             :cohort-id "c3"
             :displaced-count 1
             :surplus-usd-micros-yr 0
             :funded false})
        ear (couple/earmark-from-surplus ev)
        out (dc/evaluate-cohort ev ear [])]
    (is (= :g2-refused (:phase out)))
    (is (false? (:admissible out)))))
