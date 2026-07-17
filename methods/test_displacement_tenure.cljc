(ns fuchi.methods.test-displacement-tenure
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-l0-path :as d]
            [fuchi.methods.displacement-gov :as g]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.itonami-bridge :as ib]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(deftest test-tenure-climb-l4-to-l6
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "cohort-robotics-remote-2026"
             :displaced-count 48
             :surplus-usd-micros-yr 120000000000
             :funded true})
        pkg (d/run-for-event ev :max-slots 1 :climb-steps 4)
        s (first (:subjects pkg))
        t (ten/tenure-climb-subject s :target-stage "L6")]
    (is (= :offline-enrolled (:phase pkg)))
    (is (= "L4" (:stage s)))
    (is (= "L6" (:stage t)))
    (is (= "L6" (get-in t [:ladder-fact :stage])))
    (is (= "care" (first (get-in t [:stage-sustenance :rails]))))
    (is (= "housing" (second (get-in t [:stage-sustenance :rails]))))
    (is (= :booked-offline (get-in t [:booking :phase])))
    (is (= :open (:disclosure-state t)))
    (is (false? (:disclosure-held? t)))
    (is (true? (:entitlements-may-flow? t)))
    (is (map? (:disclosure-hold t)))
    (is (map? (:disclosure-continuity t)))
    (is (false? (:live t)))
    (is (= 0 (:cash-usd-micros t)))
    (pp/assert-no-public-scores! (:public-person t))))

(deftest test-tenure-stale-disclosure-holds-flow
  "Stale disclosure: continuity holds flow; ladder refuses L4→L6 advance (correct)."
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "c-stale"
             :displaced-count 5
             :surplus-usd-micros-yr 120000000000
             :funded true})
        pkg (d/run-for-event ev :max-slots 1 :climb-steps 4)
        s (first (:subjects pkg))
        stale {:wage-labor-band :stale :state-benefits? false
               :wellbecoming-attest-fact :stale :related-party-edges []
               :rider-s2-self-report :none}
        t (ten/tenure-climb-subject s :target-stage "L6" :disclosure stale)
        g (g/package-subject t)]
    ;; held disclosure blocks ladder advance — stage remains L4
    (is (= "L4" (:stage t)))
    (is (= :held (:disclosure-state t)))
    (is (true? (:disclosure-held? t)))
    (is (false? (:entitlements-may-flow? t)))
    (is (true? (:disclosure-held? g)))
    (is (false? (:may-flow? g)))
    (is (zero? (get-in g [:flowable-booking :in-kind-total-usd-micros])))
    (is (false? (:live t)))
    (is (= 0 (:cash-usd-micros t)))))

(deftest test-tenure-l5
  (let [ev (couple/make-displacement-event
            {:displacing-actor "itonami-robotics"
             :cohort-id "c"
             :displaced-count 10
             :surplus-usd-micros-yr 120000000000
             :funded true})
        pkg (d/run-for-event ev :max-slots 1)
        t (ten/tenure-climb-subject (first (:subjects pkg)) :target-stage "L5")]
    (is (= "L5" (:stage t)))
    (is (= "housing" (first (get-in t [:stage-sustenance :rails]))))
    (is (some #{"care"} (get-in t [:stage-sustenance :rails])))))

#?(:clj
   (deftest test-batch-tenure
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           events (ib/load-itonami-batch seed)
           batch (d/run-from-itonami-seed seed :max-slots 1 :climb-steps 4)
           out (ten/run-batch-with-tenure batch events :target-stage "L6")]
       (is (pos? (:tenure-admissible-cohorts out)))
       (is (pos? (:tenure-subjects out)))
       (is (false? (:live out)))
       (is (= 0 (:cash-usd-micros out)))
       (let [tp (first (filter #(= :tenure-offline (:tenure-phase %)) (:packages out)))
             ts (first (:tenure-subjects tp))]
         (is (= "L6" (:stage ts)))
         (is (true? (get-in tp [:tenure-couple :admissible])))))))
