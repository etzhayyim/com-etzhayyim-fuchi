(ns fuchi.methods.test-public-person
  "Tests for as-of public-person derivation (ADR-2607177000)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.allocate :as allocate]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

(def ^:private full-disclosure
  {:wage-labor-band "0-10h"
   :state-benefits? false
   :wellbecoming-attest-fact :submitted
   :related-party-edges []
   :rider-s2-self-report :none})

(defn- vowed-recipient []
  {:did "did:web:etzhayyim.com:member:abel"
   :covenant "vowed"
   :rails [{:kind "food" :active? true} {:kind "energy" :active? true}]
   :floor-usd-micros-yr 4000000000
   :disclosure full-disclosure
   :exit-suspended? false})

(deftest test-priority-stack-order
  (is (= pp/PRIORITY-STACK
         [:wellbecoming :mago-wellbecoming :ko-wellbecoming :present-adherent :operational])))

(deftest test-public-person-when-covenant-and-rails
  (is (true? (pp/public-person? (vowed-recipient)))))

(deftest test-not-public-when-exit-suspended
  (is (false? (pp/public-person? (assoc (vowed-recipient) :exit-suspended? true)))))

(deftest test-not-public-without-rails-or-floor
  (is (false? (pp/public-person?
               {:did "x" :covenant "vowed" :rails [] :floor-usd-micros-yr 0
                :exit-suspended? false}))))

(deftest test-outreach-with-rails-is-public
  (is (true? (pp/public-person?
              {:did "y" :covenant "outreach"
               :rails [{:kind "food" :active? true}]
               :floor-usd-micros-yr 1000
               :exit-suspended? false}))))

(deftest test-disclosure-gate-hold-on-missing
  (let [p (assoc (vowed-recipient) :disclosure {})
        g (pp/disclosure-gate p)]
    (is (false? (:admissible g)))
    (is (= :hold (:action g)))))

(deftest test-disclosure-gate-pass-when-fresh
  (let [g (pp/disclosure-gate (vowed-recipient))]
    (is (true? (:admissible g)))
    (is (= :pass (:action g)))))

(deftest test-public-surface-has-no-score-keys
  (let [alloc (allocate/make-allocation
               {:maintainer-did "did:web:etzhayyim.com:member:abel"
                :instrument "sustenance"
                :weight 1.2 :share 0.5 :priority-rank 1
                :floor-usd-micros-yr 4000000000})
        surf (pp/public-surface (vowed-recipient) :allocation alloc :stage "L2")]
    (is (true? (:public-person? surf)))
    (is (nil? (:priority-rank surf)))
    (is (nil? (:rank surf)))
    (is (nil? (:share surf)))
    (is (nil? (:weight surf)))
    (is (nil? (:score surf)))
    (is (= [] (:score-surface surf)))
    (is (pos? (:imputed-fact surf)))
    (is (pp/assert-no-public-scores! surf))))

(deftest test-strip-score-keys-removes-rank
  (let [m (pp/strip-score-keys {:did "x" :priority-rank 3 :score 99 :imputed-fact 1})]
    (is (= "x" (:did m)))
    (is (= 1 (:imputed-fact m)))
    (is (nil? (:priority-rank m)))
    (is (nil? (:score m)))))

(deftest test-internal-rationing-separate-from-public
  (let [alloc {:weight 1.0 :priority-rank 2 :share 0.3}
        internal (pp/internal-rationing alloc)
        surf (pp/public-surface (vowed-recipient) :allocation alloc)]
    (is (= 2 (:priority-rank internal)))
    (is (= :internal (:surface internal)))
    (is (nil? (:priority-rank surf)))))

(deftest test-prefer-mago-over-present
  (is (= :a (pp/prefer
             {:wellbecoming 0 :mago-wellbecoming 1 :ko-wellbecoming 0 :present-adherent 0}
             {:wellbecoming 0 :mago-wellbecoming 0 :ko-wellbecoming 0 :present-adherent 100})))
  (is (= :b (pp/prefer
             {:wellbecoming 0 :mago-wellbecoming 0 :ko-wellbecoming 0 :present-adherent 50}
             {:wellbecoming 1 :mago-wellbecoming 0 :ko-wellbecoming 0 :present-adherent 0}))))

(deftest test-score-forbidden-denylist-nonempty
  (is (seq pp/SCORE-FORBIDDEN-KEYS))
  (is (contains? pp/SCORE-FORBIDDEN-KEYS :priority-rank))
  (is (contains? pp/SCORE-FORBIDDEN-KEYS :wellbecoming-score)))

#?(:clj
   (deftest test-edn-ssot-matches-code
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           edn (edn/load-edn (io/file actor "data" "public-person-dynamic.edn"))
           stack (get edn ":def/priority-stack")
           stack-k (mapv (fn [x]
                           (keyword
                            (let [s (str x)]
                              (if (clojure.string/starts-with? s ":") (subs s 1) s))))
                         (or stack []))]
       (is (= pp/PRIORITY-STACK stack-k))
       (is (= "2607177000" (str (get edn ":def/adr"))))
       (let [surfaces (get edn ":def/surfaces")
             score-surf (or (get surfaces ":score") [])]
         (is (empty? score-surf) "SCORE surface must be empty in SSoT")))))
