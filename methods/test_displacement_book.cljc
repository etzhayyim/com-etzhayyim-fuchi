(ns fuchi.methods.test-displacement-book
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.displacement-book :as db]
            [fuchi.methods.stage-sustenance :as st]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(deftest test-book-l3-subject
  (let [p {:did "did:web:etzhayyim.com:displaced:c:w0" :covenant "vowed"
           :rails [] :floor-usd-micros-yr 0 :disclosure fresh
           :exit-suspended? false :stage "L3" :cash-usd-micros 0}
        hm (dh/initial p)
        sp (st/build-for-stage p hm)
        booked (db/book-subject (:did p) sp)]
    (is (= :booked-offline (:phase booked)))
    (is (pos? (:entry-count booked)))
    (is (contains? (:category-counts booked) "subsistence-flow"))
    (is (contains? (:category-counts booked) "care-flow"))
    (is (contains? (:category-counts booked) "vocation-flow"))
    (is (every? #(= 0 (:cash-usd-micros %)) (:ledger-entries booked)))
    (is (false? (:write-live-admissible booked)))
    (is (false? (:live booked)))
    (is (seq (:flow-edges booked)))
    (is (some #(= "publicfund-to-fuchi" (:flow-class %)) (:flow-edges booked)))
    (pp/assert-no-public-scores! (db/public-book-summary booked))))

(deftest test-book-flowable-omits-held
  (let [p {:did "did:web:etzhayyim.com:displaced:c:w1" :covenant "vowed"
           :rails [] :floor-usd-micros-yr 0 :disclosure
           {:wage-labor-band "0-10h" :state-benefits? false
            :wellbecoming-attest-fact :submitted :related-party-edges []
            :rider-s2-self-report :none}
           :exit-suspended? false :stage "L4" :cash-usd-micros 0}
        hm (dh/initial p)
        sp (st/build-for-stage p hm)
        flow {"care" true "food" true "energy" true "housing" false
              "tooling" true "compute" true}
        booked (db/book-flowable (:did p) sp flow)]
    (is (pos? (:entry-count booked)))
    (is (not-any? #(= "housing-commons" %) (:rails booked)))
    (is (some #(= "care-iyashi" %) (:rails booked)))
    (is (false? (:write-live-admissible booked)))
    (is (= 0 (:cash-usd-micros booked)))))
