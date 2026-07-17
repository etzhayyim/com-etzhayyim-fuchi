(ns fuchi.methods.test-rail-compute-murakumo
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.rail-compute-murakumo :as m]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "compute" :active? true}] :floor-usd-micros-yr 800000000
   :disclosure d :exit-suspended? false})

(deftest test-compute-r1
  (let [i (m/r1-dry-intent "a" 800000000)]
    (is (= "compute-murakumo" (:rail-kind i)))
    (is (= "murakumo" (:provider-did i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))))

(deftest test-compute-package-and-hold
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})]
    (is (= :R1-dry (:phase pkg)))
    (is (= [] (:score-surface pkg)))
    (pp/assert-no-public-scores! pkg))
  (let [p (person {:wage-labor-band :stale :state-benefits? false
                   :wellbecoming-attest-fact :stale :related-party-edges []
                   :rider-s2-self-report :none})
        pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person p :hold-machine (dh/initial p)})]
    (is (= :refused (:phase pkg)))))

(deftest test-gated-plan
  (is (false? (get (m/default-refuse-status) "admissible")))
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (m/gated-live-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-live-plan (:phase plan)))
    (is (false? (:live plan)))
    (is (false? (:published plan)))))
