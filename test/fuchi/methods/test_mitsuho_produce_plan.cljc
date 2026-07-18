(ns fuchi.methods.test-mitsuho-produce-plan
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.mitsuho-produce-plan :as mp]
            [fuchi.methods.rail-mitsuho :as m]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person []
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "food" :active? true}] :floor-usd-micros-yr 2000000000
   :disclosure fresh :exit-suspended? false})

(deftest test-plan-from-r1
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 2000000000 :person (person)})
        plan (mp/plan-from-r1 pkg)]
    (is (= :dry-produce-plan (:phase plan)))
    (is (false? (:produce-executed plan)))
    (is (false? (:live plan)))
    (is (pos? (:kcal-floor-yr plan)))
    (is (= 0 (:cash-usd-micros plan)))
    (is (= [] (:score-surface plan)))
    (pp/assert-no-public-scores! plan)))

(deftest test-gated-produce-not-executed
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000000 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (mp/gated-produce-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-produce-plan (:phase plan)))
    (is (false? (:produce-executed plan)))
    (is (false? (:live plan)))
    (is (false? (:published plan)))))

(deftest test-gated-produce-status-default-refuse
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000000 :person (person)})
        st (mp/gated-produce-status pkg)]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:produce-executed st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (pp/assert-no-public-scores! st)))

(deftest test-gated-produce-status-with-capability
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000000 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        st (mp/gated-produce-status pkg :gate gate
                                    :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-produce-plan (:phase st)))
    (is (true? (:admissible st)))
    (is (false? (:produce-executed st)))
    (is (false? (:live st)))
    (is (pos? (:kcal-floor-yr st)))
    (pp/assert-no-public-scores! st)))
