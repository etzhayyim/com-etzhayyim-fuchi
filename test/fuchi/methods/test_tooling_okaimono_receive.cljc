(ns fuchi.methods.test-tooling-okaimono-receive
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.tooling-okaimono-receive :as tr]
            [fuchi.methods.rail-tooling-okaimono :as t]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person []
  {:did "did:web:etzhayyim.com:member:seth" :covenant "vowed"
   :rails [{:kind "tooling" :active? true}] :floor-usd-micros-yr 1000
   :disclosure fresh :exit-suspended? false})

(deftest test-dry-receive
  (let [pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000 :person (person)})
        ack (tr/receive-from-r1-package pkg)]
    (is (= :dry-ack (:phase ack)))
    (is (false? (:fulfillment-invoked ack)))
    (is (false? (:live ack)))
    (is (= 0 (:cash-usd-micros ack)))
    (pp/assert-no-public-scores! ack)))

(deftest test-gated-no-fulfill
  (let [pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        ack (tr/gated-receive-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-ack-plan (:phase ack)))
    (is (false? (:fulfillment-invoked ack)))
    (is (false? (:live ack)))))
