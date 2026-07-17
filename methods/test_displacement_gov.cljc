(ns fuchi.methods.test-displacement-gov
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.displacement-gov :as g]
            [fuchi.methods.displacement-l0-path :as d]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.live-gate :as live-gate]))

(deftest test-housing-routes-council
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "c1"
             :displaced-count 10
             :surplus-usd-micros-yr 120000000000
             :funded true})
        pkg (d/run-for-event ev :max-slots 1 :climb-steps 4)
        s (first (:subjects pkg))
        r (g/package-subject s)]
    (is (= :offline-enrolled (:phase pkg)))
    (is (= "council-lv7" (:route r)))
    (is (true? (:invariant-touch r)))
    (is (= :council-pending-offline (get-in r [:gov-package :phase])))
    (is (true? (:entitlements-held? r)))
    (is (true? (:may-flow? r)))
    (is (true? (:may-flow-substrate? r)))
    (is (false? (get-in r [:rail-flow "housing"])))
    (is (true? (get-in r [:rail-flow "care"])))
    (is (true? (get-in r [:rail-flow "food"])))
    (is (true? (get-in r [:rail-flow "energy"])))
    (is (some #{"housing"} (:held-rails r)))
    (is (some #{"care"} (:flow-rails r)))
    (is (str/includes? (str (:entitlement-hold-reason r)) "housing-held"))
    (is (map? (:flowable-booking r)))
    (is (not-any? #(= "housing-commons" %) (get-in r [:flowable-booking :rails])))
    (is (some #(= "care-iyashi" %) (get-in r [:flowable-booking :rails])))
    (is (= :refused (get-in r [:r2-by-rail "housing" :phase])))
    (let [rat (g/council-ratify-plan r)]
      (is (= :council-ratify-plan (:phase rat)))
      (is (true? (:housing-released-offline rat)))
      (is (false? (:land-grant-executed rat)))
      (is (true? (get-in rat [:rail-flow-after "housing"])))
      (is (false? (:live rat))))
    (let [rr (g/apply-council-ratify-rebook r s)]
      (is (map? (:council-ratify rr)))
      (is (true? (:post-ratify-includes-housing rr)))
      (is (some #(= "housing-commons" %) (get-in rr [:post-ratify-booking :rails])))
      (is (false? (:land-grant-executed rr)))
      (is (< (get-in r [:flowable-booking :in-kind-total-usd-micros])
             (get-in rr [:post-ratify-booking :in-kind-total-usd-micros]))))
    (is (false? (:live r)))
    (is (= 0 (:cash-usd-micros r)))
    (pp/assert-no-public-scores! (dissoc r :gov-package :rail-flow :r2-by-rail :flowable-booking))))

(deftest test-sbt-vote-package-not-finalized
  (let [route {:subject-did "did:x" :route "sbt-vote" :committed-usd-micros-yr 30000000000}
        v (g/open-sbt-vote-package route :opened-at 1000 :now 1000)]
    (is (= :vote-open-offline (:phase v)))
    (is (= "pending" (:outcome v)))
    (is (false? (:finalizable v)))
    (is (false? (:finalize-binding-admissible v)))
    (is (false? (get (live-gate/gate-status
                      (live-gate/make-live-gate {:leg "vote"}) {})
                     "admissible")))))

(deftest test-package-batch
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "c2"
             :displaced-count 5
             :surplus-usd-micros-yr 120000000000
             :funded true})
        pkg (d/run-for-event ev :max-slots 1)
        batch {:packages [pkg] :path "t"}
        out (g/package-batch batch)
        gp (first (:packages out))]
    (is (pos? (get-in out [:gov-route-counts "council-lv7"] 0)))
    (is (pos? (:gov-entitlements-held gp)))
    (is (pos? (:gov-entitlements-may-flow gp)))
    (is (pos? (:gov-substrate-may-flow gp)))
    (is (pos? (:gov-flowable-committed-usd-micros gp)))
    (is (pos? (:gov-post-ratify-committed-usd-micros gp)))
    (is (< (:gov-flowable-committed-usd-micros gp)
           (:gov-post-ratify-committed-usd-micros gp)))
    (is (map? (:couple-pre-gov gp)))
    (is (map? (:couple gp)))
    (is (map? (:couple-post-ratify gp)))
    (is (true? (get-in gp [:couple :admissible])))
    (is (<= (get-in gp [:couple :committed-usd-micros-yr])
            (get-in gp [:couple-post-ratify :committed-usd-micros-yr])))
    (is (some? (get-in gp [:subjects 0 :flowable-booking])))
    (is (true? (:gov-packaged? out)))
    (is (= out (g/package-batch out)))
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))))

(deftest test-auto-may-flow
  (let [hold (g/entitlement-hold-for-route "auto")]
    (is (false? (:entitlements-held? hold)))
    (is (true? (:may-flow? hold)))
    (is (true? (:may-flow-substrate? hold)))))

(deftest test-council-partial-hold
  (let [hold (g/entitlement-hold-for-route "council-lv7"
                                           ["care" "food" "energy" "housing" "tooling"])]
    (is (true? (:entitlements-held? hold)))
    (is (true? (:may-flow? hold)))
    (is (true? (:may-flow-substrate? hold)))
    (is (= ["housing"] (:held-rails hold)))
    (is (false? (get-in hold [:rail-flow "housing"])))
    (is (true? (get-in hold [:rail-flow "care"])))))
