(ns fuchi.methods.test-tooling-okaimono-produce-plan
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.tooling-okaimono-produce-plan :as tp]
            [fuchi.methods.rail-tooling-okaimono :as t]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person []
  {:did "did:web:etzhayyim.com:member:seth" :covenant "vowed"
   :rails [{:kind "tooling" :active? true}] :floor-usd-micros-yr 500000000
   :disclosure fresh :exit-suspended? false})

(deftest test-plan-from-r1
  (let [pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 500000000 :person (person)})
        plan (tp/plan-from-r1 pkg)]
    (is (= :dry-produce-plan (:phase plan)))
    (is (false? (:fulfillment-executed plan)))
    (is (false? (:live plan)))
    (is (pos? (:tool-units-floor-yr plan)))
    (is (= 0 (:cash-usd-micros plan)))
    (pp/assert-no-public-scores! plan)))

(deftest test-gated-not-executed
  (let [pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000000 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (tp/gated-produce-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-produce-plan (:phase plan)))
    (is (false? (:fulfillment-executed plan)))))
