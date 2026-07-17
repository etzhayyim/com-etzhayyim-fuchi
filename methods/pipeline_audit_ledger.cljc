(ns fuchi.methods.pipeline-audit-ledger
  "pipeline_audit_ledger.cljc — append-only offline audit of displacement SS pipeline runs.

  Each line is one EDN map (facts only). No personal scores. cash≡0. live=false.
  Does not execute produce/book/couple live. Portable .cljc; I/O at #?(:clj) edge."
  (:require [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.string :as str])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn event-from-pipeline
  "Project a pipeline run! result into one audit event (no nested batch bodies)."
  [pipeline-result & {:keys [run-id note]}]
  (let [sc (:scorecard pipeline-result)
        ev {:audit/id (or run-id (str "run-" (hash (str (System/currentTimeMillis)
                                                          (:scorecard/committed-usd-micros-yr sc)))))
            :audit/pipeline (or (:pipeline pipeline-result) "displacement-ss-offline")
            :audit/ts-ms #?(:clj (System/currentTimeMillis) :cljs 0)
            :audit/admissible-cohorts (or (:admissible-cohorts pipeline-result)
                                          (:scorecard/admissible-cohorts sc) 0)
            :audit/refused-cohorts (or (:scorecard/refused-cohorts sc) 0)
            :audit/enrolled-subjects (or (:scorecard/enrolled-subjects sc) 0)
            :audit/tenure-subjects (or (:tenure-subjects pipeline-result)
                                       (:scorecard/tenure-subjects sc) 0)
            :audit/tenure-stages (or (:scorecard/tenure-stage-counts sc) {})
            :audit/committed-usd-micros-yr (or (:scorecard/committed-usd-micros-yr sc) 0)
            :audit/headroom-usd-micros-yr (or (:scorecard/headroom-usd-micros-yr sc) 0)
            :audit/booked-entries (or (:scorecard/booked-entries sc) 0)
            :audit/tenure-booked-entries (or (:scorecard/tenure-booked-entries sc) 0)
            :audit/all-live-refused (boolean (or (:all-live-refused pipeline-result)
                                                 (:scorecard/all-live-refused sc)))
            :audit/gov-route-counts (or (:gov-route-counts pipeline-result)
                                        (:scorecard/gov-route-counts sc)
                                        {})
            :audit/gov-flowable-committed-usd-micros
            (or (:scorecard/gov-flowable-committed-usd-micros sc) 0)
            :audit/gov-post-ratify-committed-usd-micros
            (or (:scorecard/gov-post-ratify-committed-usd-micros sc) 0)
            :audit/tenure-gov-flowable-committed-usd-micros
            (or (:scorecard/tenure-gov-flowable-committed-usd-micros sc) 0)
            :audit/tenure-gov-post-ratify-committed-usd-micros
            (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc) 0)
            :audit/l4-disclosure-open (or (:scorecard/l4-disclosure-open sc) 0)
            :audit/l4-disclosure-held (or (:scorecard/l4-disclosure-held sc) 0)
            :audit/tenure-disclosure-open (or (:scorecard/tenure-disclosure-open sc) 0)
            :audit/tenure-disclosure-held (or (:scorecard/tenure-disclosure-held sc) 0)
            ;; multi-gen substrate R1→gated-live design facts (executed always 0 offline)
            :audit/mitsuho-r1-dry (or (:scorecard/mitsuho-r1-dry sc) 0)
            :audit/mitsuho-gated-refused (or (:scorecard/mitsuho-gated-refused sc) 0)
            :audit/mitsuho-produce-executed (or (:scorecard/mitsuho-produce-executed sc) 0)
            :audit/hikari-r1-dry (or (:scorecard/hikari-r1-dry sc) 0)
            :audit/hikari-gated-refused (or (:scorecard/hikari-gated-refused sc) 0)
            :audit/hikari-generate-executed (or (:scorecard/hikari-generate-executed sc) 0)
            :audit/care-r1-dry (or (:scorecard/care-r1-dry sc) 0)
            :audit/care-gated-refused (or (:scorecard/care-gated-refused sc) 0)
            :audit/care-delivery-executed (or (:scorecard/care-delivery-executed sc) 0)
            :audit/housing-r1-dry (or (:scorecard/housing-r1-dry sc) 0)
            :audit/housing-gated-refused (or (:scorecard/housing-gated-refused sc) 0)
            :audit/housing-land-grant-executed (or (:scorecard/housing-land-grant-executed sc) 0)
            :audit/housing-council-held (or (:scorecard/housing-council-held sc) 0)
            :audit/tooling-r1-dry (or (:scorecard/tooling-r1-dry sc) 0)
            :audit/tooling-gated-refused (or (:scorecard/tooling-gated-refused sc) 0)
            :audit/tooling-fulfillment-executed (or (:scorecard/tooling-fulfillment-executed sc) 0)
            :audit/compute-r1-dry (or (:scorecard/compute-r1-dry sc) 0)
            :audit/compute-gated-refused (or (:scorecard/compute-gated-refused sc) 0)
            :audit/compute-quota-executed (or (:scorecard/compute-quota-executed sc) 0)
            :audit/liquidity-r1-dry (or (:scorecard/liquidity-r1-dry sc) 0)
            :audit/liquidity-gated-refused (or (:scorecard/liquidity-gated-refused sc) 0)
            :audit/liquidity-loan-executed (or (:scorecard/liquidity-loan-executed sc) 0)
            :audit/liquidity-member-principal (or (:scorecard/liquidity-member-principal sc) 0)
            :audit/liquidity-cash-usd-micros (or (:scorecard/liquidity-cash-usd-micros sc) 0)
            :audit/r2-status-count (or (:scorecard/r2-status-count sc) 0)
            :audit/r2-refused (or (:scorecard/r2-refused sc) 0)
            :audit/r2-executed (or (:scorecard/r2-executed sc) 0)
            :audit/all-r2-not-executed
            (boolean (or (:scorecard/all-r2-not-executed sc)
                         (zero? (or (:scorecard/r2-executed sc) 0))))
            ;; SS priority path (L0 + disclosure + all-rails gated) embedded facts
            :audit/ss-rails-gated-count
            (or (get-in sc [:scorecard/ss-priority-path :rails-gated-count]) 0)
            :audit/ss-rails-gated-admissible-count
            (or (get-in sc [:scorecard/ss-priority-path :rails-gated-admissible-count]) 0)
            :audit/ss-all-rails-gated-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :all-rails-gated-refused] true))
            :audit/ss-r2-status-count
            (or (get-in sc [:scorecard/ss-priority-path :r2-status-count]) 0)
            :audit/ss-r2-executed-count
            (or (get-in sc [:scorecard/ss-priority-path :r2-executed-count]) 0)
            :audit/ss-all-r2-not-executed
            (boolean (get-in sc [:scorecard/ss-priority-path :all-r2-not-executed] true))
            :audit/ss-l0-published
            (boolean (get-in sc [:scorecard/ss-priority-path :l0-published]))
            :audit/ss-ladder-to
            (or (get-in sc [:scorecard/ss-priority-path :ladder-to]) "n/a")
            :audit/ss-ladder-steps
            (or (get-in sc [:scorecard/ss-priority-path :ladder-steps]) 0)
            :audit/ss-ladder-rails-hint-first
            (or (get-in sc [:scorecard/ss-priority-path :ladder-rails-hint-first]) "n/a")
            :audit/ss-ladder-published
            (boolean (get-in sc [:scorecard/ss-priority-path :ladder-published]))
            :audit/ss-held-stress-ladder-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :held-stress-ladder-refused]))
            :audit/ss-stage-rails-first
            (or (get-in sc [:scorecard/ss-priority-path :stage-rails-first]) "n/a")
            :audit/ss-stage-rails-second
            (or (get-in sc [:scorecard/ss-priority-path :stage-rails-second]) "n/a")
            :audit/ss-stage-r2-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-r2-all-refused] true))
            :audit/ss-stage-all-gated-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-all-gated-refused] true))
            :audit/ss-stage-gated-count
            (or (get-in sc [:scorecard/ss-priority-path :stage-gated-count]) 0)
            :audit/ss-stage-care-gated-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-care-gated-admissible]))
            :audit/ss-stage-mitsuho-gated-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-mitsuho-gated-admissible]))
            :audit/ss-stage-hikari-gated-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-hikari-gated-admissible]))
            :audit/ss-stage-land-grant-executed
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-land-grant-executed]))
            :audit/ss-disclosure-state
            (or (get-in sc [:scorecard/ss-priority-path :disclosure-state]) "n/a")
            :audit/ss-housing-land-grant-executed
            (boolean (get-in sc [:scorecard/ss-priority-path :housing-land-grant-executed]))
            :audit/ss-mitsuho-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :mitsuho-gated-receive-admissible]))
            :audit/ss-hikari-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :hikari-gated-receive-admissible]))
            :audit/ss-care-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :care-gated-receive-admissible]))
            :audit/ss-mitsuho-hikari-receive-both-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :mitsuho-hikari-receive-both-refused]
                             true))
            :audit/ss-care-mitsuho-hikari-receive-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-mitsuho-hikari-receive-all-refused]
                             true))
            :audit/ss-mitsuho-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :mitsuho-gated-produce-admissible]))
            :audit/ss-hikari-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :hikari-gated-produce-admissible]))
            :audit/ss-mitsuho-hikari-produce-both-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :mitsuho-hikari-produce-both-refused]
                             true))
            :audit/ss-mitsuho-hikari-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :mitsuho-hikari-full-chain-refused]
                             true))
            :audit/ss-care-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-gated-produce-admissible]))
            :audit/ss-care-mitsuho-hikari-produce-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-mitsuho-hikari-produce-all-refused]
                             true))
            :audit/ss-care-mitsuho-hikari-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-mitsuho-hikari-full-chain-refused]
                             true))
            :audit/all-held-stress-gov-flowable
            (or (get-in sc [:scorecard/all-held-stress :gov-flowable]) 0)
            :audit/all-held-stress-held-subjects
            (or (get-in sc [:scorecard/all-held-stress :held-subjects]) 0)
            :audit/cash-usd-micros 0
            :audit/cash-to-workers-usd-micros 0
            :audit/live false
            :audit/score-surface []
            :audit/priority-stack PRIORITY-STACK
            :audit/note (or note "offline pipeline audit — no live side-effects")}]
    (pp/assert-no-public-scores! ev)
    ev))

#?(:clj
   (defn ledger-path
     ([]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))]
        (io/file actor "out" "pipeline-audit-ledger.ednl")))
     ([actor-dir]
      (io/file actor-dir "out" "pipeline-audit-ledger.ednl"))))

#?(:clj
   (defn append!
     "Append one event line to out/pipeline-audit-ledger.ednl. Returns path + event."
     [event]
     (let [f (ledger-path)
           _ (.mkdirs (.getParentFile f))
           line (str (pr-str event) "\n")]
       (spit f line :append true)
       {:path (str f)
        :event event
        :live false
        :cash-usd-micros 0
        :score-surface []
        :priority-stack PRIORITY-STACK
        :deployed false})))

#?(:clj
   (defn append-from-pipeline!
     "Append audit line from a pipeline run result."
     [pipeline-result & opts]
     (append! (apply event-from-pipeline pipeline-result opts))))

#?(:clj
   (defn read-all
     "Read all audit events (vector). Empty if missing."
     []
     (let [f (ledger-path)]
       (if-not (.exists f)
         []
         (->> (str/split-lines (slurp f))
              (remove str/blank?)
              (mapv read-string))))))

(defn- last-run-snapshot
  "Facts-only projection of the most recent audit event (no scores)."
  [ev]
  (when ev
    (let [out {:run-id (:audit/id ev)
               :enrolled-subjects (or (:audit/enrolled-subjects ev) 0)
               :tenure-subjects (or (:audit/tenure-subjects ev) 0)
               :gov-flowable-committed-usd-micros
               (or (:audit/gov-flowable-committed-usd-micros ev) 0)
               :gov-post-ratify-committed-usd-micros
               (or (:audit/gov-post-ratify-committed-usd-micros ev) 0)
               :tenure-gov-flowable-committed-usd-micros
               (or (:audit/tenure-gov-flowable-committed-usd-micros ev) 0)
               :tenure-gov-post-ratify-committed-usd-micros
               (or (:audit/tenure-gov-post-ratify-committed-usd-micros ev) 0)
               :housing-land-grant-executed
               (or (:audit/housing-land-grant-executed ev) 0)
               :housing-council-held (or (:audit/housing-council-held ev) 0)
               :liquidity-member-principal
               (or (:audit/liquidity-member-principal ev) 0)
               :liquidity-cash-usd-micros
               (or (:audit/liquidity-cash-usd-micros ev) 0)
               :r2-status-count (or (:audit/r2-status-count ev) 0)
               :r2-refused (or (:audit/r2-refused ev) 0)
               :r2-executed (or (:audit/r2-executed ev) 0)
               :all-r2-not-executed
               (boolean (or (:audit/all-r2-not-executed ev)
                            (zero? (or (:audit/r2-executed ev) 0))))
               :ss-rails-gated-count (or (:audit/ss-rails-gated-count ev) 0)
               :ss-all-rails-gated-refused
               (boolean (:audit/ss-all-rails-gated-refused ev true))
               :ss-r2-status-count (or (:audit/ss-r2-status-count ev) 0)
               :ss-r2-executed-count (or (:audit/ss-r2-executed-count ev) 0)
               :ss-all-r2-not-executed
               (boolean (:audit/ss-all-r2-not-executed ev true))
               :ss-l0-published (boolean (:audit/ss-l0-published ev))
               :ss-ladder-to (or (:audit/ss-ladder-to ev) "n/a")
               :ss-ladder-steps (or (:audit/ss-ladder-steps ev) 0)
               :ss-ladder-rails-hint-first
               (or (:audit/ss-ladder-rails-hint-first ev) "n/a")
               :ss-held-stress-ladder-refused
               (boolean (:audit/ss-held-stress-ladder-refused ev))
               :ss-stage-rails-first
               (or (:audit/ss-stage-rails-first ev) "n/a")
               :ss-stage-rails-second
               (or (:audit/ss-stage-rails-second ev) "n/a")
               :ss-stage-gated-count
               (or (:audit/ss-stage-gated-count ev) 0)
               :ss-stage-all-gated-refused
               (boolean (:audit/ss-stage-all-gated-refused ev true))
               :ss-stage-r2-all-refused
               (boolean (:audit/ss-stage-r2-all-refused ev true))
               :ss-stage-care-gated-admissible
               (boolean (:audit/ss-stage-care-gated-admissible ev))
               :ss-stage-mitsuho-gated-admissible
               (boolean (:audit/ss-stage-mitsuho-gated-admissible ev))
               :ss-stage-hikari-gated-admissible
               (boolean (:audit/ss-stage-hikari-gated-admissible ev))
               :ss-stage-land-grant-executed
               (boolean (:audit/ss-stage-land-grant-executed ev))
               :ss-mitsuho-gated-receive-admissible
               (boolean (:audit/ss-mitsuho-gated-receive-admissible ev))
               :ss-hikari-gated-receive-admissible
               (boolean (:audit/ss-hikari-gated-receive-admissible ev))
               :ss-care-gated-receive-admissible
               (boolean (:audit/ss-care-gated-receive-admissible ev))
               :ss-mitsuho-hikari-receive-both-refused
               (boolean (:audit/ss-mitsuho-hikari-receive-both-refused ev true))
               :ss-care-mitsuho-hikari-receive-all-refused
               (boolean (:audit/ss-care-mitsuho-hikari-receive-all-refused ev true))
               :ss-mitsuho-gated-produce-admissible
               (boolean (:audit/ss-mitsuho-gated-produce-admissible ev))
               :ss-hikari-gated-produce-admissible
               (boolean (:audit/ss-hikari-gated-produce-admissible ev))
               :ss-mitsuho-hikari-produce-both-refused
               (boolean (:audit/ss-mitsuho-hikari-produce-both-refused ev true))
               :ss-mitsuho-hikari-full-chain-refused
               (boolean (:audit/ss-mitsuho-hikari-full-chain-refused ev true))
               :ss-care-gated-produce-admissible
               (boolean (:audit/ss-care-gated-produce-admissible ev))
               :ss-care-mitsuho-hikari-produce-all-refused
               (boolean (:audit/ss-care-mitsuho-hikari-produce-all-refused ev true))
               :ss-care-mitsuho-hikari-full-chain-refused
               (boolean (:audit/ss-care-mitsuho-hikari-full-chain-refused ev true))
               :ss-disclosure-state (or (:audit/ss-disclosure-state ev) "n/a")
               :all-live-refused (boolean (:audit/all-live-refused ev))
               :l4-disclosure-open (or (:audit/l4-disclosure-open ev) 0)
               :l4-disclosure-held (or (:audit/l4-disclosure-held ev) 0)
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK}]
      (pp/assert-no-public-scores! out)
      out)))

#?(:clj
   (defn summary
     "Aggregate facts across ledger (no scores).
      Includes last-run post-ratify/flowable snapshot (USD micros are not summed across runs —
      each run is a full offline recompute, so last-run is the authoritative latest package)."
     ([]
      (summary (read-all)))
     ([events]
      (let [last-ev (last events)
            empty? (empty? events)
            out {:runs (count events)
                 :total-enrolled (reduce + 0 (map :audit/enrolled-subjects events))
                 :total-tenure (reduce + 0 (map :audit/tenure-subjects events))
                 :total-l4-disclosure-open
                 (reduce + 0 (map #(or (:audit/l4-disclosure-open %) 0) events))
                 :total-l4-disclosure-held
                 (reduce + 0 (map #(or (:audit/l4-disclosure-held %) 0) events))
                 :total-tenure-disclosure-open
                 (reduce + 0 (map #(or (:audit/tenure-disclosure-open %) 0) events))
                 :total-tenure-disclosure-held
                 (reduce + 0 (map #(or (:audit/tenure-disclosure-held %) 0) events))
                 :total-mitsuho-gated-refused
                 (reduce + 0 (map #(or (:audit/mitsuho-gated-refused %) 0) events))
                 :total-hikari-gated-refused
                 (reduce + 0 (map #(or (:audit/hikari-gated-refused %) 0) events))
                 :total-care-gated-refused
                 (reduce + 0 (map #(or (:audit/care-gated-refused %) 0) events))
                 :total-housing-gated-refused
                 (reduce + 0 (map #(or (:audit/housing-gated-refused %) 0) events))
                 :total-tooling-gated-refused
                 (reduce + 0 (map #(or (:audit/tooling-gated-refused %) 0) events))
                 :total-compute-gated-refused
                 (reduce + 0 (map #(or (:audit/compute-gated-refused %) 0) events))
                 :total-liquidity-gated-refused
                 (reduce + 0 (map #(or (:audit/liquidity-gated-refused %) 0) events))
                 :total-liquidity-member-principal
                 (reduce + 0 (map #(or (:audit/liquidity-member-principal %) 0) events))
                 :total-liquidity-cash-usd-micros
                 (reduce + 0 (map #(or (:audit/liquidity-cash-usd-micros %) 0) events))
                 :total-housing-land-grant-executed
                 (reduce + 0 (map #(or (:audit/housing-land-grant-executed %) 0) events))
                 :all-runs-live-refused (if empty? true (every? :audit/all-live-refused events))
                 :any-land-grant-executed?
                 (boolean (some #(pos? (or (:audit/housing-land-grant-executed %) 0)) events))
                 ;; last offline package facts (post-ratify vs flowable; land-grant stays 0)
                 :last-run (last-run-snapshot last-ev)
                 :last-run-gov-flowable-committed-usd-micros
                 (or (:audit/gov-flowable-committed-usd-micros last-ev) 0)
                 :last-run-gov-post-ratify-committed-usd-micros
                 (or (:audit/gov-post-ratify-committed-usd-micros last-ev) 0)
                 :last-run-tenure-gov-flowable-committed-usd-micros
                 (or (:audit/tenure-gov-flowable-committed-usd-micros last-ev) 0)
                 :last-run-tenure-gov-post-ratify-committed-usd-micros
                 (or (:audit/tenure-gov-post-ratify-committed-usd-micros last-ev) 0)
                 :last-run-housing-land-grant-executed
                 (or (:audit/housing-land-grant-executed last-ev) 0)
                 :last-run-r2-refused (or (:audit/r2-refused last-ev) 0)
                 :last-run-r2-executed (or (:audit/r2-executed last-ev) 0)
                 :last-run-all-r2-not-executed
                 (boolean (or (:audit/all-r2-not-executed last-ev)
                              (zero? (or (:audit/r2-executed last-ev) 0))))
                 :last-run-ss-rails-gated-count
                 (or (:audit/ss-rails-gated-count last-ev) 0)
                 :last-run-ss-all-rails-gated-refused
                 (boolean (:audit/ss-all-rails-gated-refused last-ev true))
                 :last-run-ss-r2-status-count
                 (or (:audit/ss-r2-status-count last-ev) 0)
                 :last-run-ss-all-r2-not-executed
                 (boolean (:audit/ss-all-r2-not-executed last-ev true))
                 :last-run-ss-l0-published
                 (boolean (:audit/ss-l0-published last-ev))
                 :last-run-ss-ladder-to
                 (or (:audit/ss-ladder-to last-ev) "n/a")
                 :last-run-ss-ladder-rails-hint-first
                 (or (:audit/ss-ladder-rails-hint-first last-ev) "n/a")
                 :last-run-ss-held-stress-ladder-refused
                 (boolean (:audit/ss-held-stress-ladder-refused last-ev))
                 :last-run-ss-stage-rails-first
                 (or (:audit/ss-stage-rails-first last-ev) "n/a")
                 :last-run-ss-stage-rails-second
                 (or (:audit/ss-stage-rails-second last-ev) "n/a")
                 :last-run-ss-stage-gated-count
                 (or (:audit/ss-stage-gated-count last-ev) 0)
                 :last-run-ss-stage-all-gated-refused
                 (boolean (:audit/ss-stage-all-gated-refused last-ev true))
                 :last-run-ss-stage-r2-all-refused
                 (boolean (:audit/ss-stage-r2-all-refused last-ev true))
                 :last-run-ss-stage-care-gated-admissible
                 (boolean (:audit/ss-stage-care-gated-admissible last-ev))
                 :last-run-ss-stage-mitsuho-gated-admissible
                 (boolean (:audit/ss-stage-mitsuho-gated-admissible last-ev))
                 :last-run-ss-stage-hikari-gated-admissible
                 (boolean (:audit/ss-stage-hikari-gated-admissible last-ev))
                 :last-run-ss-stage-land-grant-executed
                 (boolean (:audit/ss-stage-land-grant-executed last-ev))
                 :last-run-ss-mitsuho-gated-receive-admissible
                 (boolean (:audit/ss-mitsuho-gated-receive-admissible last-ev))
                 :last-run-ss-hikari-gated-receive-admissible
                 (boolean (:audit/ss-hikari-gated-receive-admissible last-ev))
                 :last-run-ss-care-gated-receive-admissible
                 (boolean (:audit/ss-care-gated-receive-admissible last-ev))
                 :last-run-ss-mitsuho-hikari-receive-both-refused
                 (boolean (:audit/ss-mitsuho-hikari-receive-both-refused last-ev true))
                 :last-run-ss-care-mitsuho-hikari-receive-all-refused
                 (boolean (:audit/ss-care-mitsuho-hikari-receive-all-refused last-ev true))
                 :last-run-ss-mitsuho-gated-produce-admissible
                 (boolean (:audit/ss-mitsuho-gated-produce-admissible last-ev))
                 :last-run-ss-hikari-gated-produce-admissible
                 (boolean (:audit/ss-hikari-gated-produce-admissible last-ev))
                 :last-run-ss-mitsuho-hikari-produce-both-refused
                 (boolean (:audit/ss-mitsuho-hikari-produce-both-refused last-ev true))
                 :last-run-ss-mitsuho-hikari-full-chain-refused
                 (boolean (:audit/ss-mitsuho-hikari-full-chain-refused last-ev true))
                 :last-run-ss-care-gated-produce-admissible
                 (boolean (:audit/ss-care-gated-produce-admissible last-ev))
                 :last-run-ss-care-mitsuho-hikari-produce-all-refused
                 (boolean (:audit/ss-care-mitsuho-hikari-produce-all-refused last-ev true))
                 :last-run-ss-care-mitsuho-hikari-full-chain-refused
                 (boolean (:audit/ss-care-mitsuho-hikari-full-chain-refused last-ev true))
                 :cash-usd-micros 0
                 :cash-to-workers-usd-micros 0
                 :live false
                 :score-surface []
                 :priority-stack PRIORITY-STACK}]
        (pp/assert-no-public-scores! (dissoc out :last-run))
        (when-let [lr (:last-run out)] (pp/assert-no-public-scores! lr))
        out))))
