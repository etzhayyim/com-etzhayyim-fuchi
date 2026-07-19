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

(deftest test-flowable-preferred-over-full-booking
  (let [ev (couple/make-displacement-event
            {:displacing-actor "x" :cohort-id "c4"
             :displaced-count 1 :surplus-usd-micros-yr 20000000000 :funded true})
        ear (couple/earmark-from-surplus ev)
        ;; full book would exceed earmark; flowable under earmark
        subjects [{:booking {:in-kind-total-usd-micros 50000000000}
                   :flowable-booking {:in-kind-total-usd-micros 5000000000}}]
        out (dc/evaluate-cohort ev ear subjects)]
    (is (true? (:admissible out)))
    (is (= 5000000000 (:committed-usd-micros-yr out)))
    (is (= 50000000000 (:committed-full-usd-micros-yr out)))
    (is (false? (:admissible-if-full-booked out)))))

(deftest test-reevaluate-after-gov-prefers-flowable
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics" :cohort-id "c5"
             :displaced-count 2 :surplus-usd-micros-yr 100000000000 :funded true})
        ear (couple/earmark-from-surplus ev)
        subjects [{:booking {:in-kind-total-usd-micros 30000000000}
                   :stage-sustenance {:floor-usd-micros-yr 30000000000}}]
        govs [{:flowable-booking {:in-kind-total-usd-micros 12000000000}
               :post-ratify-booking {:in-kind-total-usd-micros 30000000000
                                     :rails ["care-iyashi" "housing-commons"]}
               :post-ratify-includes-housing true}]
        out (dc/reevaluate-after-gov ev ear subjects govs)]
    (is (map? (:flowable-booking (first (:subjects out)))))
    (is (map? (:post-ratify-booking (first (:subjects out)))))
    (is (true? (:admissible (:couple out))))
    (is (= 12000000000 (get-in out [:couple :committed-usd-micros-yr])))
    (is (= :committed-offline-post-ratify-plan (get-in out [:couple-post-ratify :phase])))
    (is (= 30000000000 (get-in out [:couple-post-ratify :committed-usd-micros-yr])))
    (is (false? (get-in out [:couple-post-ratify :land-grant-executed])))
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))
    (pp/assert-no-public-scores! (dc/public-couple-summary (:couple out)))))
