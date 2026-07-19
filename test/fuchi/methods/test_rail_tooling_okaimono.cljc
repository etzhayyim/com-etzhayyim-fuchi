(ns fuchi.methods.test-rail-tooling-okaimono
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.rail-tooling-okaimono :as t]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:seth" :covenant "vowed"
   :rails [{:kind "tooling" :active? true}] :floor-usd-micros-yr 500000000
   :disclosure d :exit-suspended? false})

(deftest test-tooling-r1
  (let [i (t/r1-dry-intent "a" 500000000)]
    (is (= "tooling-okaimono" (:rail-kind i)))
    (is (= "did:web:etzhayyim.com:actor:okaimono" (:provider-did i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))))

(deftest test-tooling-package-and-hold
  (let [pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})]
    (is (= :R1-dry (:phase pkg)))
    (is (= [] (:score-surface pkg)))
    (pp/assert-no-public-scores! pkg))
  (let [p (person {:wage-labor-band :stale :state-benefits? false
                   :wellbecoming-attest-fact :stale :related-party-edges []
                   :rider-s2-self-report :none})
        pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person p :hold-machine (dh/initial p)})]
    (is (= :refused (:phase pkg)))))

(deftest test-gated-plan
  (is (false? (get (t/default-refuse-status) "admissible")))
  (let [pkg (t/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})
        st (t/gated-live-status pkg)
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (t/gated-live-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:fulfillment-executed st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (is (= :gated-live-plan (:phase plan)))
    (is (false? (:live plan)))
    (is (false? (:published plan)))
    (pp/assert-no-public-scores! st)))
