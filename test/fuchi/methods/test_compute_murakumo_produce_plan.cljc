(ns fuchi.methods.test-compute-murakumo-produce-plan
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.compute-murakumo-produce-plan :as cp]
            [fuchi.methods.rail-compute-murakumo :as m]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person []
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "compute" :active? true}] :floor-usd-micros-yr 800000000
   :disclosure fresh :exit-suspended? false})

(deftest test-plan-from-r1
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 800000000 :person (person)})
        plan (cp/plan-from-r1 pkg)]
    (is (= :dry-produce-plan (:phase plan)))
    (is (false? (:quota-executed plan)))
    (is (false? (:live plan)))
    (is (pos? (:gpu-hours-floor-yr plan)))
    (is (= 0 (:cash-usd-micros plan)))
    (pp/assert-no-public-scores! plan)))

(deftest test-gated-not-executed
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000000 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (cp/gated-produce-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-produce-plan (:phase plan)))
    (is (false? (:quota-executed plan)))))
