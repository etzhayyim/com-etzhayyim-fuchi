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
                          :scorecard/housing-council-held 2
                          :scorecard/r2-status-count 12
                          :scorecard/r2-refused 12
                          :scorecard/r2-executed 0
                          :scorecard/all-r2-not-executed true
                          :scorecard/ss-priority-path
                          {:rails-gated-count 7
                           :rails-gated-admissible-count 0
                           :all-rails-gated-refused true
                           :r2-status-count 7
                           :r2-executed-count 0
                           :all-r2-not-executed true
                           :l0-published false
                           :disclosure-state "open"
                           :housing-land-grant-executed false}}}
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
    (is (= 12 (:audit/r2-status-count ev)))
    (is (= 12 (:audit/r2-refused ev)))
    (is (= 0 (:audit/r2-executed ev)))
    (is (true? (:audit/all-r2-not-executed ev)))
    (is (= 7 (:audit/ss-rails-gated-count ev)))
    (is (true? (:audit/ss-all-rails-gated-refused ev)))
    (is (= 7 (:audit/ss-r2-status-count ev)))
    (is (zero? (:audit/ss-r2-executed-count ev)))
    (is (true? (:audit/ss-all-r2-not-executed ev)))
    (is (false? (:audit/ss-l0-published ev)))
    (is (= "open" (:audit/ss-disclosure-state ev)))
    (is (false? (:audit/ss-housing-land-grant-executed ev)))
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
       ;; last-run post-ratify / flowable parity (USD micros not summed across runs)
       (is (pos? (:last-run-gov-flowable-committed-usd-micros sum)))
       (is (pos? (:last-run-gov-post-ratify-committed-usd-micros sum)))
       (is (>= (:last-run-gov-post-ratify-committed-usd-micros sum)
               (:last-run-gov-flowable-committed-usd-micros sum)))
       (is (pos? (:last-run-tenure-gov-post-ratify-committed-usd-micros sum)))
       (is (zero? (:last-run-housing-land-grant-executed sum)))
       (is (pos? (:last-run-r2-refused sum)))
       (is (zero? (:last-run-r2-executed sum)))
       (is (true? (:last-run-all-r2-not-executed sum)))
       (is (pos? (:last-run-ss-rails-gated-count sum)))
       (is (true? (:last-run-ss-all-rails-gated-refused sum)))
       (is (true? (:last-run-ss-all-r2-not-executed sum)))
       (is (false? (:last-run-ss-l0-published sum)))
       (is (= "L4" (:last-run-ss-ladder-to sum)))
       (is (= "care" (:last-run-ss-stage-rails-first sum)))
       (is (= "housing" (:last-run-ss-stage-rails-second sum)))
       (is (pos? (:last-run-ss-stage-gated-count sum)))
       (is (true? (:last-run-ss-stage-all-gated-refused sum)))
       (is (true? (:last-run-ss-stage-r2-all-refused sum)))
       (is (false? (:last-run-ss-stage-care-gated-admissible sum)))
       (is (false? (:last-run-ss-stage-mitsuho-gated-admissible sum)))
       (is (false? (:last-run-ss-stage-hikari-gated-admissible sum)))
       (is (false? (:last-run-ss-stage-land-grant-executed sum)))
       (is (false? (:last-run-ss-mitsuho-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-hikari-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-care-gated-receive-admissible sum)))
       (is (true? (:last-run-ss-mitsuho-hikari-receive-both-refused sum)))
       (is (true? (:last-run-ss-care-mitsuho-hikari-receive-all-refused sum)))
       (is (false? (:last-run-ss-mitsuho-gated-produce-admissible sum)))
       (is (false? (:last-run-ss-hikari-gated-produce-admissible sum)))
       (is (true? (:last-run-ss-mitsuho-hikari-produce-both-refused sum)))
       (is (true? (:last-run-ss-mitsuho-hikari-full-chain-refused sum)))
       (is (map? (:last-run sum)))
       (is (true? (get-in sum [:last-run :ss-stage-all-gated-refused])))
       (is (false? (get-in sum [:last-run :ss-stage-care-gated-admissible])))
       (is (true? (get-in sum [:last-run :ss-care-mitsuho-hikari-receive-all-refused])))
       (is (true? (get-in sum [:last-run :ss-mitsuho-hikari-full-chain-refused])))
       (is (zero? (get-in sum [:last-run :housing-land-grant-executed])))
       (is (zero? (get-in sum [:last-run :r2-executed])))
       (is (false? (get-in sum [:last-run :live])))
       (is (zero? (:total-liquidity-cash-usd-micros sum)))
       (is (= 0 (:cash-usd-micros sum)))
       (is (false? (:live sum))))))
