(ns fuchi.methods.test-hikari-receive
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.hikari-receive :as hr]
            [fuchi.methods.rail-hikari :as h]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person []
  {:did "did:web:etzhayyim.com:member:seth" :covenant "vowed"
   :rails [{:kind "energy" :active? true}] :floor-usd-micros-yr 1000
   :disclosure fresh :exit-suspended? false})

(deftest test-dry-receive
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000 :person (person)})
        ack (hr/receive-from-r1-package pkg)]
    (is (= :dry-ack (:phase ack)))
    (is (false? (:generate-invoked ack)))
    (is (false? (:live ack)))
    (is (= 0 (:cash-usd-micros ack)))
    (pp/assert-no-public-scores! ack)))

(deftest test-gated-no-generate
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        ack (hr/gated-receive-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-ack-plan (:phase ack)))
    (is (false? (:generate-invoked ack)))
    (is (false? (:live ack)))))

(deftest test-gated-receive-status-default-refuse
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000 :person (person)})
        st (hr/gated-receive-status pkg)]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:generate-invoked st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (pp/assert-no-public-scores! st)))

(deftest test-gated-receive-status-with-capability
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        st (hr/gated-receive-status pkg :gate gate
                                    :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-ack-plan (:phase st)))
    (is (true? (:admissible st)))
    (is (false? (:generate-invoked st)))
    (is (false? (:live st)))
    (pp/assert-no-public-scores! st)))
