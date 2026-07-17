(ns fuchi.methods.test-pipeline-audit-ledger
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.displacement-pipeline :as pipe])
            #?(:clj [clojure.java.io :as io])))

(deftest test-event-from-pipeline-shape
  (let [fake {:pipeline "displacement-ss-offline"
              :admissible-cohorts 1
              :tenure-subjects 2
              :all-live-refused true
              :scorecard {:scorecard/admissible-cohorts 1
                          :scorecard/refused-cohorts 1
                          :scorecard/enrolled-subjects 2
                          :scorecard/tenure-subjects 2
                          :scorecard/tenure-stage-counts {"L6" 2}
                          :scorecard/committed-usd-micros-yr 100
                          :scorecard/headroom-usd-micros-yr 50
                          :scorecard/booked-entries 12
                          :scorecard/tenure-booked-entries 12
                          :scorecard/all-live-refused true
                          :scorecard/gov-flowable-committed-usd-micros 40
                          :scorecard/gov-post-ratify-committed-usd-micros 100
                          :scorecard/tenure-gov-flowable-committed-usd-micros 40
                          :scorecard/tenure-gov-post-ratify-committed-usd-micros 100
                          :scorecard/l4-disclosure-open 2
                          :scorecard/l4-disclosure-held 0
                          :scorecard/tenure-disclosure-open 2
                          :scorecard/tenure-disclosure-held 0
                          :scorecard/mitsuho-r1-dry 2
                          :scorecard/mitsuho-gated-refused 2
                          :scorecard/mitsuho-produce-executed 0
                          :scorecard/hikari-r1-dry 2
                          :scorecard/hikari-gated-refused 2
                          :scorecard/hikari-generate-executed 0
                          :scorecard/care-r1-dry 2
                          :scorecard/care-gated-refused 2
                          :scorecard/care-delivery-executed 0
                          :scorecard/housing-r1-dry 2
                          :scorecard/housing-gated-refused 2
                          :scorecard/housing-land-grant-executed 0
                          :scorecard/housing-council-held 2}}
        ev (audit/event-from-pipeline fake :run-id "test-run-1")]
    (is (= "test-run-1" (:audit/id ev)))
    (is (true? (:audit/all-live-refused ev)))
    (is (= 40 (:audit/gov-flowable-committed-usd-micros ev)))
    (is (= 100 (:audit/gov-post-ratify-committed-usd-micros ev)))
    (is (= 2 (:audit/l4-disclosure-open ev)))
    (is (= 0 (:audit/l4-disclosure-held ev)))
    (is (= 2 (:audit/tenure-disclosure-open ev)))
    (is (= 2 (:audit/mitsuho-gated-refused ev)))
    (is (= 2 (:audit/care-gated-refused ev)))
    (is (= 2 (:audit/housing-gated-refused ev)))
    (is (= 0 (:audit/housing-land-grant-executed ev)))
    (is (= 0 (:audit/mitsuho-produce-executed ev)))
    (is (= 0 (:audit/cash-usd-micros ev)))
    (is (= 0 (:audit/cash-to-workers-usd-micros ev)))
    (is (false? (:audit/live ev)))
    (is (= [] (:audit/score-surface ev)))
    (pp/assert-no-public-scores! ev)))

#?(:clj
   (deftest test-append-and-summary
     (let [result (pipe/run! :max-slots 1)
           a (audit/append-from-pipeline! result :run-id (str "t-" (System/currentTimeMillis)))
           events (audit/read-all)
           sum (audit/summary)
           last-ev (last events)]
       (is (.exists (io/file (:path a))))
       (is (seq events))
       (is (pos? (:runs sum)))
       (is (true? (:all-runs-live-refused sum)))
       (is (pos? (:total-l4-disclosure-open sum)))
       (is (pos? (:total-mitsuho-gated-refused sum)))
       (is (pos? (:total-care-gated-refused sum)))
       (is (pos? (:total-housing-gated-refused sum)))
       (is (false? (:any-land-grant-executed? sum)))
       (is (pos? (get last-ev :audit/gov-flowable-committed-usd-micros 0)))
       (is (pos? (get last-ev :audit/mitsuho-gated-refused 0)))
       (is (zero? (get last-ev :audit/housing-land-grant-executed 0)))
       (is (= 0 (:cash-usd-micros sum)))
       (is (false? (:live sum))))))
