(ns fuchi.methods.test-stage-sustenance
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.stage-sustenance :as st]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [stage]
  {:did "did:web:etzhayyim.com:displaced:c:w0" :covenant "vowed"
   :rails [{:kind "food" :active? true}]
   :floor-usd-micros-yr 0
   :disclosure fresh :exit-suspended? false :stage stage :cash-usd-micros 0})

(deftest test-l2-includes-housing
  (let [p (person "L2")
        hm (dh/initial p)
        pkg (st/build-for-stage p hm)]
    (is (= "L2" (:stage pkg)))
    (is (some #{"housing"} (:rails pkg)))
    (is (some #{"care"} (:rails pkg)))
    (is (some #{"food"} (:rails pkg)))
    (is (some #{"energy"} (:rails pkg)))
    (is (get-in pkg [:packages "housing" :plan]))
    (is (false? (get-in pkg [:packages "housing" :plan :land-grant-executed])))
    (is (pos? (get-in pkg [:packages "housing" :floor :housing-months-floor-yr])))
    (is (= :refused (get-in pkg [:packages "food" :r2 :phase])))
    (is (false? (:live pkg)))
    (is (= 0 (:cash-usd-micros pkg)))
    (pp/assert-no-public-scores! (st/public-floor-row pkg))))

(deftest test-l0-care-only-hint
  (let [p (person "L0")
        hm (dh/initial p)
        pkg (st/build-for-stage p hm)]
    (is (= ["care"] (:rails pkg)))
    (is (get-in pkg [:packages "care" :plan]))))

(deftest test-l3-vocation-plus-substrate
  (let [p (person "L3")
        hm (dh/initial p)
        pkg (st/build-for-stage p hm)]
    (is (= "L3" (:stage pkg)))
    (is (some #{"tooling"} (:rails pkg)))
    (is (some #{"compute"} (:rails pkg)))
    (is (some #{"housing"} (:rails pkg)))
    (is (some #{"care"} (:rails pkg)))
    (is (get-in pkg [:packages "tooling" :plan]))
    (is (get-in pkg [:packages "compute" :plan]))
    (is (false? (get-in pkg [:packages "tooling" :plan :fulfillment-executed])))
    (is (= :refused (get-in pkg [:packages "compute" :r2 :phase])))))

(deftest test-l4-multi-gen-first
  (let [p (person "L4")
        hm (dh/initial p)
        pkg (st/build-for-stage p hm)
        rails (:rails pkg)]
    (is (= "L4" (:stage pkg)))
    (is (= "care" (first rails)))
    (is (= "housing" (second rails)))
    (is (some #{"compute"} rails))
    (is (get-in pkg [:packages "care" :plan]))
    (is (get-in pkg [:packages "housing" :plan]))))

(deftest test-l6-multi-gen-first
  (let [p (person "L6")
        hm (dh/initial p)
        pkg (st/build-for-stage p hm)
        rails (:rails pkg)]
    (is (= "L6" (:stage pkg)))
    (is (= "care" (first rails)))
    (is (= "housing" (second rails)))
    (is (= 6 (count rails)))))
