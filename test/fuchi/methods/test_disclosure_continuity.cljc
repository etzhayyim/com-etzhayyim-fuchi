(ns fuchi.methods.test-disclosure-continuity
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.disclosure-continuity :as dc]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(def ^:private stale
  {:wage-labor-band :stale :state-benefits? false
   :wellbecoming-attest-fact :stale :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "food" :active? true}] :floor-usd-micros-yr 1000
   :disclosure d :exit-suspended? false :stage "L2" :cash-usd-micros 0})

(deftest test-tick-fresh-stays-open
  (let [p (person fresh)
        m (dh/initial p)
        t (dc/tick m p)]
    (is (= :open (:to-state t)))
    (is (false? (:held? t)))
    (is (true? (dc/entitlements-may-flow? (:machine t))))
    (is (false? (:live t)))
    (is (= 0 (:cash-usd-micros t)))
    (is (= [] (:score-surface t)))))

(deftest test-tick-stale-holds
  (let [p (person fresh)
        m (dh/initial p)
        t (dc/tick m p :disclosure stale)]
    (is (= :held (:to-state t)))
    (is (true? (:held? t)))
    (is (false? (dc/entitlements-may-flow? (:machine t))))
    (is (= :held-on-stale (:action t)))))

(deftest test-series-reopen
  (let [p (person fresh)
        out (dc/tick-series p [fresh stale fresh])]
    (is (= :open (:final-state out)))
    (is (= 3 (count (:history out))))
    (is (false? (:live out)))
    (pp/assert-no-public-scores! (select-keys out [:live :cash-usd-micros :score-surface]))))
