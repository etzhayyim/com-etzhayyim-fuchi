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

#?(:clj
   (defn summary
     "Aggregate facts across ledger (no scores)."
     ([]
      (summary (read-all)))
     ([events]
      (let [out {:runs (count events)
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
                 :total-housing-land-grant-executed
                 (reduce + 0 (map #(or (:audit/housing-land-grant-executed %) 0) events))
                 :all-runs-live-refused (every? :audit/all-live-refused events)
                 :any-land-grant-executed?
                 (boolean (some #(pos? (or (:audit/housing-land-grant-executed %) 0)) events))
                 :cash-usd-micros 0
                 :cash-to-workers-usd-micros 0
                 :live false
                 :score-surface []
                 :priority-stack PRIORITY-STACK}]
        (pp/assert-no-public-scores! out)
        out))))
