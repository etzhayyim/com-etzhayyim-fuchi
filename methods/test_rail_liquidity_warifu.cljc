(ns fuchi.methods.test-rail-liquidity-warifu
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.rail-liquidity-warifu :as w]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:seth" :covenant "vowed"
   :rails [{:kind "liquidity" :active? true}] :floor-usd-micros-yr 3000000000
   :disclosure d :exit-suspended? false})

(deftest test-liquidity-r1-member-principal
  (let [i (w/r1-dry-intent "a" 3000000000)]
    (is (= "liquidity-warifu" (:rail-kind i)))
    (is (= "did:web:etzhayyim.com:actor:warifu" (:provider-did i)))
    (is (true? (:member-principal i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))))

(deftest test-liquidity-package-and-hold
  (let [pkg (w/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})]
    (is (= :R1-dry (:phase pkg)))
    (is (true? (:member-principal pkg)))
    (is (= [] (:score-surface pkg)))
    (pp/assert-no-public-scores! pkg))
  (let [p (person {:wage-labor-band :stale :state-benefits? false
                   :wellbecoming-attest-fact :stale :related-party-edges []
                   :rider-s2-self-report :none})
        pkg (w/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person p :hold-machine (dh/initial p)})]
    (is (= :refused (:phase pkg)))))

(deftest test-gated-plan
  (is (false? (get (w/default-refuse-status) "admissible")))
  (let [pkg (w/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (w/gated-live-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-live-plan (:phase plan)))
    (is (true? (:member-principal plan)))
    (is (false? (:live plan)))
    (is (= 0 (:cash-usd-micros plan)))))
