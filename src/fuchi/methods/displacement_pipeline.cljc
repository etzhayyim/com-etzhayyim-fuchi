(ns fuchi.methods.displacement-pipeline
  "displacement_pipeline.cljc — single offline entry for robotics/itonami SS path.

  L0 enroll → disclosure → L4 multi-gen floors → book → G2 headroom
  → optional L6 tenure → G7 package → scorecard facts.
  write-all! optionally refreshes public/ package (Pages plan-only, never deploys).
  live=false throughout. cash≡0. Portable .cljc; file I/O at #?(:clj) edge."
  (:require [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-scorecard :as sc]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.displacement-gov :as dgov]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.pages-deploy :as pages-dep])
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

#?(:clj
   (defn run!
     "Run full offline pipeline. Options: max-slots, climb-steps (L4=4), tenure-target (L6)."
     [& {:keys [max-slots climb-steps tenure-target include-tenure]
         :or {max-slots 2 climb-steps 4 tenure-target "L6" include-tenure true}}]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (io/file "."))
           seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           events (itonami/load-itonami-batch seed)
           batch (dl0/run-from-itonami-seed seed :max-slots max-slots :climb-steps climb-steps)
           batch2 (if include-tenure
                    (ten/run-batch-with-tenure batch events :target-stage tenure-target)
                    batch)
           batch3 (dgov/package-batch batch2)
           scorecard (sc/build batch3)
           out {:pipeline "displacement-ss-offline"
                :live false
                :cash-usd-micros 0
                :score-surface []
                :priority-stack PRIORITY-STACK
                :batch batch3
                :scorecard scorecard
                :gov-route-counts (:gov-route-counts batch3)
                :admissible-cohorts (:scorecard/admissible-cohorts scorecard)
                :tenure-subjects (:scorecard/tenure-subjects scorecard)
                :all-live-refused (:scorecard/all-live-refused scorecard)
                ;; surface G7 package facts at top-level (facts only)
                :gov-flowable-committed-usd-micros
                (or (:scorecard/gov-flowable-committed-usd-micros scorecard) 0)
                :gov-post-ratify-committed-usd-micros
                (or (:scorecard/gov-post-ratify-committed-usd-micros scorecard) 0)
                :tenure-gov-flowable-committed-usd-micros
                (or (:scorecard/tenure-gov-flowable-committed-usd-micros scorecard) 0)
                :tenure-gov-post-ratify-committed-usd-micros
                (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros scorecard) 0)
                :housing-land-grant-executed
                (or (:scorecard/housing-land-grant-executed scorecard) 0)
                :housing-council-held
                (or (:scorecard/housing-council-held scorecard) 0)
                :r2-status-count (or (:scorecard/r2-status-count scorecard) 0)
                :r2-refused (or (:scorecard/r2-refused scorecard) 0)
                :r2-executed (or (:scorecard/r2-executed scorecard) 0)
                :all-r2-not-executed
                (boolean (or (:scorecard/all-r2-not-executed scorecard)
                             (zero? (or (:scorecard/r2-executed scorecard) 0))))
                :ss-priority-path (or (:scorecard/ss-priority-path scorecard) {})
                :ss-all-rails-gated-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :all-rails-gated-refused]
                                 true))
                :ss-all-r2-not-executed
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :all-r2-not-executed]
                                 true))
                :ss-ladder-to
                (or (get-in scorecard [:scorecard/ss-priority-path :ladder-to]) "n/a")
                :ss-stage-rails-first
                (or (get-in scorecard [:scorecard/ss-priority-path :stage-rails-first]) "n/a")
                :ss-stage-all-gated-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-all-gated-refused]
                                 true))
                :ss-stage-r2-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-r2-all-refused]
                                 true))
                :ss-stage-care-gated-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-care-gated-admissible]))
                :ss-stage-mitsuho-gated-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-mitsuho-gated-admissible]))
                :ss-stage-hikari-gated-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-hikari-gated-admissible]))
                :ss-stage-land-grant-executed
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-land-grant-executed]))
                :ss-mitsuho-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-gated-receive-admissible]))
                :ss-hikari-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :hikari-gated-receive-admissible]))
                :ss-care-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-gated-receive-admissible]))
                :ss-mitsuho-hikari-receive-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-hikari-receive-both-refused]
                                 true))
                :ss-care-mitsuho-hikari-receive-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-mitsuho-hikari-receive-all-refused]
                                 true))
                :ss-mitsuho-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-gated-produce-admissible]))
                :ss-hikari-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :hikari-gated-produce-admissible]))
                :ss-mitsuho-hikari-produce-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-hikari-produce-both-refused]
                                 true))
                :ss-mitsuho-hikari-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-hikari-full-chain-refused]
                                 true))
                :ss-care-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-gated-produce-admissible]))
                :ss-care-mitsuho-hikari-produce-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-mitsuho-hikari-produce-all-refused]
                                 true))
                :ss-care-mitsuho-hikari-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-mitsuho-hikari-full-chain-refused]
                                 true))
                :ss-housing-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :housing-gated-receive-admissible]))
                :ss-housing-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :housing-gated-produce-admissible]))
                :ss-housing-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :housing-full-chain-refused]
                                 true))
                :ss-care-housing-mitsuho-hikari-receive-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-housing-mitsuho-hikari-receive-all-refused]
                                 true))
                :ss-care-housing-mitsuho-hikari-produce-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-housing-mitsuho-hikari-produce-all-refused]
                                 true))
                :ss-care-housing-mitsuho-hikari-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-housing-mitsuho-hikari-full-chain-refused]
                                 true))}]
       (pp/assert-no-public-scores!
        (select-keys out [:live :cash-usd-micros :score-surface :priority-stack
                          :admissible-cohorts :tenure-subjects :all-live-refused
                          :gov-flowable-committed-usd-micros
                          :gov-post-ratify-committed-usd-micros
                          :housing-land-grant-executed
                          :r2-executed :r2-refused :all-r2-not-executed
                          :ss-all-rails-gated-refused :ss-all-r2-not-executed
                          :ss-stage-all-gated-refused :ss-stage-r2-all-refused
                          :ss-stage-land-grant-executed
                          :ss-stage-care-gated-admissible
                          :ss-stage-mitsuho-gated-admissible
                          :ss-stage-hikari-gated-admissible
                          :ss-mitsuho-gated-receive-admissible
                          :ss-hikari-gated-receive-admissible
                          :ss-care-gated-receive-admissible
                          :ss-mitsuho-hikari-receive-both-refused
                          :ss-care-mitsuho-hikari-receive-all-refused
                          :ss-mitsuho-gated-produce-admissible
                          :ss-hikari-gated-produce-admissible
                          :ss-mitsuho-hikari-produce-both-refused
                          :ss-mitsuho-hikari-full-chain-refused
                          :ss-care-gated-produce-admissible
                          :ss-care-mitsuho-hikari-produce-all-refused
                          :ss-care-mitsuho-hikari-full-chain-refused
                          :ss-housing-gated-receive-admissible
                          :ss-housing-gated-produce-admissible
                          :ss-housing-full-chain-refused
                          :ss-care-housing-mitsuho-hikari-receive-all-refused
                          :ss-care-housing-mitsuho-hikari-produce-all-refused
                          :ss-care-housing-mitsuho-hikari-full-chain-refused]))
       out)))

#?(:clj
   (defn write-all!
     "Run pipeline + write scorecard + append audit + optionally refresh public/ package.

     Options (in addition to run!):
       :include-public — default true; calls pages-deploy/write-deploy-package!
         (static package only; deployed=false; never wrangler/API).
       :deploy-env / :operator-did / :project-name — forwarded to deploy package writer.

     Never deploys. live=false. cash≡0."
     [& {:keys [include-public deploy-env operator-did project-name]
         :or {include-public true}
         :as opts}]
     (let [opts (or opts {})
           run-opts (dissoc opts :include-public :deploy-env :operator-did :project-name)
           result (apply run! (mapcat identity run-opts))
           ;; rebuild scorecard files from same batch to avoid double heavy seed run
           scard (:scorecard result)
           actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (io/file "."))
           outd (io/file actor "out")
           _ (.mkdirs outd)
           _ (spit (io/file outd "displacement-scorecard.md") (sc/scorecard-md scard))
           _ (spit (io/file outd "displacement-scorecard.edn") (pr-str scard))
           paths {:md (str (io/file outd "displacement-scorecard.md"))
                  :edn (str (io/file outd "displacement-scorecard.edn"))}
           audit-out (audit/append-from-pipeline! result)
           public-pkg (when include-public
                        (try
                          (pages-dep/write-deploy-package!
                           (cond-> {}
                             deploy-env (assoc :env deploy-env)
                             operator-did (assoc :operator-did operator-did)
                             project-name (assoc :project-name project-name)))
                          (catch Exception e
                            {:error (.getMessage e)
                             :deployed false
                             :live false
                             :cash-usd-micros 0
                             :package-ready false})))
           out (cond-> (assoc result
                              :paths paths
                              :audit audit-out
                              :live false
                              :cash-usd-micros 0
                              :score-surface []
                              :deployed false
                              :wrangler-invoked false
                              :package-ready (boolean (and include-public
                                                           public-pkg
                                                           (not (:error public-pkg))
                                                           (true? (:package-ready public-pkg true))))
                              :priority-stack PRIORITY-STACK)
                 public-pkg (assoc :public-package public-pkg
                                   :deploy-status (:deploy-status public-pkg)
                                   :deployed (boolean (:deployed public-pkg false))
                                   :wrangler-invoked (boolean (:wrangler-invoked public-pkg false))))]
       (pp/assert-no-public-scores!
        (select-keys out [:live :cash-usd-micros :score-surface :deployed
                          :package-ready :wrangler-invoked
                          :housing-land-grant-executed
                          :gov-post-ratify-committed-usd-micros]))
       out)))
