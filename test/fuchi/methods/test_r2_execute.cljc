(ns fuchi.methods.test-r2-execute
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.r2-execute :as r2]
            [fuchi.methods.rail-mitsuho :as m]
            [fuchi.methods.mitsuho-produce-plan :as mp]
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

(defn- dry-plan []
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 2000000000
                               :person (person)})]
    (mp/plan-from-r1 pkg)))

(deftest test-default-refuse
  (is (false? (get (r2/default-refuse-status) "admissible")))
  (let [plan (dry-plan)
        refu (r2/refuse-without-gate "produce" plan)]
    (is (= :refused (:phase refu)))
    (is (false? (:executed refu)))
    (is (false? (:live refu)))
    (is (= 0 (:cash-usd-micros refu)))
    (pp/assert-no-public-scores! refu)))

(deftest test-gated-still-not-executed
  (let [plan (dry-plan)
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        out (r2/execute-plan "produce" plan gate
                             :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-execute-plan (:phase out)))
    (is (true? (:authorized-to-execute out)))
    (is (false? (:executed out)))
    (is (false? (:produce-executed out)))
    (is (false? (:live out)))
    (is (false? (:published out)))
    (is (= 0 (:cash-usd-micros out)))
    (pp/assert-no-public-scores! out)))

(deftest test-execute-or-refuse-without-flag
  (let [plan (dry-plan)
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        out (r2/execute-or-refuse "produce" plan gate :env {})]
    (is (= :refused (:phase out)))
    (is (false? (:executed out)))
    (is (false? (:admissible out)))))
