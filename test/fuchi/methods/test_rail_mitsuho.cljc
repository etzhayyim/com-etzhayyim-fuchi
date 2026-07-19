(ns fuchi.methods.test-rail-mitsuho
  "food-mitsuho single-rail R1 + gated-live design tests."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.rail-mitsuho :as m]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh-d
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:abel"
   :covenant "vowed"
   :rails [{:kind "food" :active? true}]
   :floor-usd-micros-yr 4000000000
   :disclosure d
   :exit-suspended? false})

(deftest test-r1-dry-intent-mitsuho
  (let [i (m/r1-dry-intent "alloc-1" 4000000000)]
    (is (= "food-mitsuho" (:rail-kind i)))
    (is (= "did:web:etzhayyim.com:actor:mitsuho" (:provider-did i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))
    (is (false? (:server-held-key i)))))

(deftest test-r1-package-open-disclosure
  (let [pkg (m/r1-dry-package {:alloc-id "a" :subject-did "did:web:etzhayyim.com:member:abel"
                               :imputed-usd-micros-yr 4000000000
                               :person (person fresh-d)})]
    (is (= :R1-dry (:phase pkg)))
    (is (true? (:public-person? pkg)))
    (is (= "open" (:disclosure-state pkg)))
    (is (= [] (:score-surface pkg)))
    (is (= 0 (:cash-usd-micros pkg)))
    (is (false? (:live pkg)))
    (pp/assert-no-public-scores! pkg)))

(deftest test-r1-refuses-when-disclosure-held
  (let [p (person {:wage-labor-band :stale :state-benefits? false
                   :wellbecoming-attest-fact :stale :related-party-edges []
                   :rider-s2-self-report :none})
        hm (dh/initial p)
        pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person p :hold-machine hm})]
    (is (= :refused (:phase pkg)))
    (is (str/includes? (str (:refusal-reason pkg)) "not open"))))

(deftest test-default-live-gate-refuses
  (let [st (m/default-refuse-status)]
    (is (false? (get st "admissible")))))

(deftest test-gated-live-status-default-refuse-no-throw
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person (person fresh-d)})
        st (m/gated-live-status pkg)]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:produce-executed st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (is (= "food-mitsuho" (:rail-kind st)))
    (pp/assert-no-public-scores! st)))

(deftest test-gated-live-plan-refuses-without-capability
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person (person fresh-d)})
        gate (live-gate/make-live-gate {:leg "provision"})]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (m/gated-live-plan pkg gate :env {})))))

(deftest test-gated-live-plan-authorizes-with-full-capability
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1000
                               :person (person fresh-d)})
        gate (live-gate/make-live-gate
              {:leg "provision"
               :operator-did "did:web:etzhayyim.com:operator:test"
               :council-level 6
               :member-signature "member-cap-ed25519-representative"})
        env {"FUCHI_ALLOW_LIVE_PROVISION" "1"}
        plan (m/gated-live-plan pkg gate :env env)]
    (is (= :gated-live-plan (:phase plan)))
    (is (true? (:authorized-to-publish plan)))
    (is (false? (:published plan)))
    (is (false? (:live plan)))
    (is (= 0 (:cash-usd-micros plan)))
    (is (= [] (:score-surface plan)))))

(deftest test-lexicon-record-shape
  (let [pkg (m/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person (person fresh-d)})
        rec (m/lexicon-record pkg)]
    (is (= "food-mitsuho" (:railKind rec)))
    (is (= false (:live rec)))
    (is (= 0 (:cashUsdMicros rec)))
    (is (= [] (:scoreSurface rec)))))
