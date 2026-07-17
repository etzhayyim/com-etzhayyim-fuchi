(ns fuchi.methods.test-mitsuho-receive
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.mitsuho-receive :as mr]
            [fuchi.methods.rail-mitsuho :as m]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person []
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "food" :active? true}] :floor-usd-micros-yr 1000
   :disclosure fresh :exit-suspended? false})

(deftest test-dry-receive-ack
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000 :person (person)})
        ack (mr/receive-from-r1-package pkg)]
    (is (= :dry-ack (:phase ack)))
    (is (false? (:produce-invoked ack)))
    (is (false? (:live ack)))
    (is (= 0 (:cash-usd-micros ack)))
    (is (= [] (:score-surface ack)))
    (pp/assert-no-public-scores! ack)))

(deftest test-rejects-non-food
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (mr/dry-receive {:rail-kind "energy-hikari"
                                :provider-did "did:web:etzhayyim.com:actor:hikari"
                                :cash-usd-micros 0 :server-held-key false
                                :published false :alloc-id "x" :imputed-usd-micros-yr 1}))))

(deftest test-gated-ack-still-no-produce
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person)})
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        ack (mr/gated-receive-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :gated-ack-plan (:phase ack)))
    (is (true? (:authorized-to-publish ack)))
    (is (false? (:produce-invoked ack)))
    (is (false? (:live ack)))
    (is (false? (:published ack)))))

(deftest test-default-refuse
  (is (false? (get (mr/default-refuse-status) "admissible"))))
