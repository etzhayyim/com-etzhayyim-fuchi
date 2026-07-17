(ns fuchi.methods.test-liberation-ladder
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.liberation-ladder :as lad]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [stage]
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "food" :active? true} {:kind "care" :active? true}]
   :floor-usd-micros-yr 3000000000
   :disclosure fresh :exit-suspended? false :stage stage :cash-usd-micros 0})

(deftest test-advance-l0-to-l1
  (let [p (person "L0")
        hm (dh/initial p)
        adv (lad/advance-offline p hm :member-signature "sig-ok")]
    (is (= :advanced-offline (:phase adv)))
    (is (= "L0" (:from-stage adv)))
    (is (= "L1" (:to-stage adv)))
    (is (false? (:live adv)))
    (is (= 0 (:cash-usd-micros adv)))
    (is (= [] (:score-surface adv)))
    (pp/assert-no-public-scores! adv)
    (let [p2 (lad/project-person p adv)]
      (is (= "L1" (:stage p2)))
      (is (seq (:multi-gen-care-facts p2))))))

(deftest test-hold-blocks-advance
  (let [p (person "L0")
        stale {:wage-labor-band :stale :state-benefits? false
               :wellbecoming-attest-fact :stale :related-party-edges []
               :rider-s2-self-report :none}
        p2 (assoc p :disclosure stale)
        hm (dh/initial p2)
        adv (lad/advance-offline p2 hm)]
    (is (= :held (:state hm)))
    (is (= :refused (:phase adv)))
    (is (false? (:live adv)))))

(deftest test-climb-two-steps
  (let [p (person "L0")
        hm (dh/initial p)
        out (lad/climb-offline p hm :steps 2 :member-signature "sig")]
    (is (= "L2" (get-in out [:person :stage])))
    (is (= 2 (count (:history out))))
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))))

(deftest test-l6-refuse
  (let [p (person "L6")
        hm (dh/initial p)
        adv (lad/advance-offline p hm)]
    (is (= :refused (:phase adv)))
    (is (nil? (lad/next-stage "L6")))))
