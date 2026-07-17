(ns fuchi.methods.test-l0-enroll
  "Offline L0 enrollment scaffold tests (ADR-2605302357 §1.16.3a + 2607177000)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.public-person :as pp]))

(def ^:private base
  {:subject-did "did:web:etzhayyim.com:member:lot"
   :vow-text "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming"
   :member-signature "sig-representative-lot-2026q2"
   :covenant "outreach"})

(deftest test-draft-requires-signature
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (l0/draft-vow (dissoc base :member-signature)))))

(deftest test-triple-permanent-offline
  (let [d (l0/draft-vow base)
        c (l0/triple-permanent d)]
    (is (= :committed-offline (:phase c)))
    (is (= "L0" (:stage c)))
    (is (= "vowed" (:covenant c)))
    (is (false? (:published c)))
    (is (= 0 (:cash-usd-micros c)))
    (is (false? (:server-held-key c)))
    (is (str/starts-with? (:kotoba-cid c) "bafy-offline-"))
    (is (str/starts-with? (:ipfs-cid c) "bafy-offline-"))
    (is (str/starts-with? (:token-id c) "sbt-offline-"))))

(deftest test-enroll-public-person-no-scores
  (let [e (l0/enroll base)
        surf (:public-person e)]
    (is (true? (:public-person? surf)))
    (is (= "L0" (:stage surf)))
    (is (= [] (:score-surface surf)))
    (is (nil? (:priority-rank surf)))
    (is (nil? (:score surf)))
    (is (= 0 (get-in e [:entitlement :cash-usd-micros])))
    (is (false? (get-in e [:entitlement :published])))
    (is (= l0/PRIORITY-STACK (:priority-stack e)))
    (pp/assert-no-public-scores! surf)))

(deftest test-enroll-record-lexicon-shape
  (let [rec (l0/enroll-record (l0/enroll base))]
    (is (= false (:published rec)))
    (is (= 0 (:cashUsdMicros rec)))
    (is (= false (:serverHeldKey rec)))
    (is (= "L0" (:stage rec)))))

(deftest test-cash-invariant
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (l0/assert-no-cash! {:cash-usd-micros 1}))))
