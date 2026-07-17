(ns fuchi.methods.displacement-pipeline
  "displacement_pipeline.cljc — single offline entry for robotics/itonami SS path.

  L0 enroll → disclosure → L4 multi-gen floors → book → G2 headroom
  → optional L6 tenure → scorecard facts. live=false throughout.
  Portable .cljc; file I/O at #?(:clj) edge."
  (:require [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-scorecard :as sc]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

#?(:clj
   (defn run!
     "Run full offline pipeline. Options: max-slots, climb-steps (L4=4), tenure-target (L6)."
     [& {:keys [max-slots climb-steps tenure-target include-tenure]
         :or {max-slots 2 climb-steps 4 tenure-target "L6" include-tenure true}}]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           events (itonami/load-itonami-batch seed)
           batch (dl0/run-from-itonami-seed seed :max-slots max-slots :climb-steps climb-steps)
           batch2 (if include-tenure
                    (ten/run-batch-with-tenure batch events :target-stage tenure-target)
                    batch)
           scorecard (sc/build batch2)
           out {:pipeline "displacement-ss-offline"
                :live false
                :cash-usd-micros 0
                :score-surface []
                :priority-stack PRIORITY-STACK
                :batch batch2
                :scorecard scorecard
                :admissible-cohorts (:scorecard/admissible-cohorts scorecard)
                :tenure-subjects (:scorecard/tenure-subjects scorecard)
                :all-live-refused (:scorecard/all-live-refused scorecard)}]
       (pp/assert-no-public-scores!
        (select-keys out [:live :cash-usd-micros :score-surface :priority-stack
                          :admissible-cohorts :tenure-subjects :all-live-refused]))
       out)))

#?(:clj
   (defn write-all!
     "Run pipeline + write scorecard + append audit ledger line under out/."
     [& opts]
     (let [result (apply run! opts)
           ;; rebuild scorecard files from same batch to avoid double heavy seed run
           scard (:scorecard result)
           actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           outd (io/file actor "out")
           _ (.mkdirs outd)
           _ (spit (io/file outd "displacement-scorecard.md") (sc/scorecard-md scard))
           _ (spit (io/file outd "displacement-scorecard.edn") (pr-str scard))
           paths {:md (str (io/file outd "displacement-scorecard.md"))
                  :edn (str (io/file outd "displacement-scorecard.edn"))}
           audit-out (audit/append-from-pipeline! result)]
       (assoc result
              :paths paths
              :audit audit-out
              :live false
              :cash-usd-micros 0
              :score-surface []
              :deployed false))))
