(ns fuchi.methods.test-displacement-gov
  (:require [clojure.test :refer [deftest is]]
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
    (is (false? (:live r)))
    (is (= 0 (:cash-usd-micros r)))
    (pp/assert-no-public-scores! r)))

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
        out (g/package-batch batch)]
    (is (pos? (get-in out [:gov-route-counts "council-lv7"] 0)))
    (is (false? (:live out)))
    (is (= 0 (:cash-usd-micros out)))))
