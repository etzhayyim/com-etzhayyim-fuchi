(ns fuchi.methods.pages-deploy
  "pages_deploy.cljc — Cloudflare Pages deploy membrane for public/ SS surface.

  Default REFUSE (no deploy). Writes a deploy-ready package (wrangler.toml stub +
  status). Even when FUCHI_ALLOW_PAGES_DEPLOY=1 + operator attestation present,
  this scaffold only returns :gated-deploy-plan — it does NOT shell out to wrangler
  or call Cloudflare APIs (side-effect execute remains out of band).

  Public surface only (facts). cash≡0. no scores. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.pages-publish :as pages]
            #?(:clj [fuchi.methods.pipeline-audit-ledger :as audit])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)
(def FLAG "FUCHI_ALLOW_PAGES_DEPLOY")
(def DEFAULT-PROJECT "fuchi-public-surface")

(defn operator-runbook-facts
  "Facts-only operator runbook for plan-only Pages packaging.
   Scaffold never deploys; wrangler/API remain out-of-band."
  ([]
   (operator-runbook-facts DEFAULT-PROJECT))
  ([project-name]
   (let [out {:flag FLAG
              :project-name project-name
              :deploy-target "cloudflare-pages"
              :required-for-gated-plan [FLAG "=1" "operator-did non-blank"]
              :scaffold-invokes-wrangler false
              :scaffold-invokes-cloudflare-api false
              :side-effect-execute "out-of-band only"
              :live-disbursement false
              :cash-usd-micros 0
              :deployed false
              :live false
              :score-surface []
              :priority-stack PRIORITY-STACK
              :steps ["write-deploy-package! → refresh public/ static facts"
                      "review index.html / facts.edn (no personal scores)"
                      (str "optional gated plan: " FLAG "=1 + operator-did"
                           " → phase=:gated-deploy-plan still deployed=false")
                      "out-of-band: wrangler pages deploy public/"
                      "never enable live sustenance disbursement from this package"]
              :note "plan-only membrane; actual deploy is operator out-of-band"}]
     (pp/assert-no-public-scores! out)
     out)))

(defn default-refuse-status
  "Bare env → not admissible."
  ([]
   (default-refuse-status {}))
  ([env]
   (let [admissible (= "1" (get env FLAG))
         reason (if admissible
                  "flag present — still requires operator plan; no auto side-effect"
                  (str "missing operator process flag '" FLAG "'"))]
     {"admissible" admissible
      "flag" FLAG
      "reason" reason
      "deployed" false
      "live" false})))

(defn refuse-deploy
  "Status map: deploy refused (default path)."
  ([]
   (refuse-deploy {}))
  ([env]
   (let [st (default-refuse-status env)
         out {:phase :refused
              :deploy-target "cloudflare-pages"
              :admissible (boolean (get st "admissible"))
              :refusal-reason (get st "reason")
              :authorized-to-deploy false
              :wrangler-invoked false
              :cloudflare-api-invoked false
              :package-ready true
              :operator-flag FLAG
              :operator-runbook (operator-runbook-facts)
              :deployed false
              :live false
              :cash-usd-micros 0
              :score-surface []
              :priority-stack PRIORITY-STACK
              :note "Pages deploy default refuse — static package only; plan-only membrane"}]
     (pp/assert-no-public-scores! (dissoc out :operator-runbook))
     (pp/assert-no-public-scores! (:operator-runbook out))
     out)))

(defn gated-deploy-plan
  "Authorize a deploy PLAN only. Does not deploy. Requires FUCHI_ALLOW_PAGES_DEPLOY=1
   and non-blank operator-did."
  [{:keys [operator-did project-name]
    :or {project-name DEFAULT-PROJECT}}
   & {:keys [env]}]
  (let [env (or env {})]
    (when-not (= "1" (get env FLAG))
      (throw (ex-info (str "missing " FLAG) {:flag FLAG})))
    (when (or (nil? operator-did) (zero? (count (str operator-did))))
      (throw (ex-info "missing operator-did attestation" {})))
    (let [out {:phase :gated-deploy-plan
               :deploy-target "cloudflare-pages"
               :project-name project-name
               :operator-did operator-did
               :operator-flag FLAG
               :authorized-to-deploy true
               :package-ready true
               :deployed false
               :wrangler-invoked false
               :cloudflare-api-invoked false
               :operator-runbook (operator-runbook-facts project-name)
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :note "gated deploy plan only — wrangler/API not invoked by scaffold; OOB deploy"}]
      (pp/assert-no-public-scores! (dissoc out :operator-runbook))
      (pp/assert-no-public-scores! (:operator-runbook out))
      out)))

(defn gated-deploy-status
  "Non-raising R1-style status for Pages deploy DESIGN.
   Default env refuses. Never invokes wrangler/Cloudflare API."
  [opts & {:keys [env]}]
  (let [env (or env {})
        operator-did (:operator-did opts)
        project-name (or (:project-name opts) DEFAULT-PROJECT)]
    (try
      (let [plan (gated-deploy-plan
                  {:operator-did operator-did :project-name project-name}
                  :env env)
            out (assoc plan
                       :admissible true
                       :score-surface []
                       :priority-stack PRIORITY-STACK)]
        (pp/assert-no-public-scores! (dissoc out :operator-runbook))
        out)
      (catch #?(:clj Exception :cljs :default) ex
        (let [st (default-refuse-status env)
              out {:phase :refused
                   :deploy-target "cloudflare-pages"
                   :project-name project-name
                   :operator-flag FLAG
                   :admissible false
                   :authorized-to-deploy false
                   :package-ready true
                   :deployed false
                   :wrangler-invoked false
                   :cloudflare-api-invoked false
                   :refusal-reason (or (ex-message ex) (get st "reason"))
                   :gate-admissible (boolean (get st "admissible"))
                   :operator-runbook (operator-runbook-facts project-name)
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :note "Pages deploy gated status — no side-effect; static package only"}]
          (pp/assert-no-public-scores! (dissoc out :operator-runbook))
          (pp/assert-no-public-scores! (:operator-runbook out))
          out)))))

(defn deploy-or-refuse
  "If flag+operator present → gated-deploy-plan; else refuse map."
  [opts & {:keys [env]}]
  (try
    (gated-deploy-plan opts :env env)
    (catch Exception e
      (assoc (refuse-deploy env)
             :refusal-reason (.getMessage e)
             :admissible false))))

(defn- audit-package-snapshot
  "Optional last-run audit facts for deploy-status package (facts only)."
  []
  #?(:clj
     (try
       (let [au (audit/summary)
             out {:runs (or (:runs au) 0)
                  :all-runs-live-refused (boolean (:all-runs-live-refused au true))
                  :any-land-grant-executed? (boolean (:any-land-grant-executed? au))
                  :last-run-gov-flowable-committed-usd-micros
                  (or (:last-run-gov-flowable-committed-usd-micros au) 0)
                  :last-run-gov-post-ratify-committed-usd-micros
                  (or (:last-run-gov-post-ratify-committed-usd-micros au) 0)
                  :last-run-tenure-gov-post-ratify-committed-usd-micros
                  (or (:last-run-tenure-gov-post-ratify-committed-usd-micros au) 0)
                  :last-run-housing-land-grant-executed
                  (or (:last-run-housing-land-grant-executed au) 0)
                  :last-run-ss-rails-gated-count
                  (or (:last-run-ss-rails-gated-count au) 0)
                  :last-run-ss-all-rails-gated-refused
                  (boolean (:last-run-ss-all-rails-gated-refused au true))
                  :last-run-ss-all-r2-not-executed
                  (boolean (:last-run-ss-all-r2-not-executed au true))
                  :last-run-ss-l0-published
                  (boolean (:last-run-ss-l0-published au))
                  :last-run-ss-ladder-to
                  (or (:last-run-ss-ladder-to au) "n/a")
                  :last-run-ss-stage-rails-first
                  (or (:last-run-ss-stage-rails-first au) "n/a")
                  :last-run-ss-stage-rails-second
                  (or (:last-run-ss-stage-rails-second au) "n/a")
                  :last-run-ss-stage-gated-count
                  (or (:last-run-ss-stage-gated-count au) 0)
                  :last-run-ss-stage-all-gated-refused
                  (boolean (:last-run-ss-stage-all-gated-refused au true))
                  :last-run-ss-stage-r2-all-refused
                  (boolean (:last-run-ss-stage-r2-all-refused au true))
                  :last-run-ss-stage-care-gated-admissible
                  (boolean (:last-run-ss-stage-care-gated-admissible au))
                  :last-run-ss-stage-mitsuho-gated-admissible
                  (boolean (:last-run-ss-stage-mitsuho-gated-admissible au))
                  :last-run-ss-stage-hikari-gated-admissible
                  (boolean (:last-run-ss-stage-hikari-gated-admissible au))
                  :last-run-ss-stage-land-grant-executed
                  (boolean (:last-run-ss-stage-land-grant-executed au))
                  :last-run-ss-mitsuho-gated-receive-admissible
                  (boolean (:last-run-ss-mitsuho-gated-receive-admissible au))
                  :last-run-ss-hikari-gated-receive-admissible
                  (boolean (:last-run-ss-hikari-gated-receive-admissible au))
                  :last-run-ss-care-gated-receive-admissible
                  (boolean (:last-run-ss-care-gated-receive-admissible au))
                  :last-run-ss-mitsuho-hikari-receive-both-refused
                  (boolean (:last-run-ss-mitsuho-hikari-receive-both-refused au true))
                  :last-run-ss-care-mitsuho-hikari-receive-all-refused
                  (boolean (:last-run-ss-care-mitsuho-hikari-receive-all-refused au true))
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK}]
         (pp/assert-no-public-scores! out)
         out)
       (catch Exception _
         {:runs 0 :live false :cash-usd-micros 0 :score-surface []
          :any-land-grant-executed? false :all-runs-live-refused true
          :priority-stack PRIORITY-STACK}))
     :cljs
     {:runs 0 :live false :cash-usd-micros 0 :score-surface []
      :any-land-grant-executed? false :all-runs-live-refused true
      :priority-stack PRIORITY-STACK}))

#?(:clj
   (defn write-deploy-package!
     "Refresh public/ via pages-publish + write wrangler.toml stub + deploy-status.edn
      + deploy-runbook.edn. Never deploys. Returns package map with deployed=false."
     ([]
      (write-deploy-package! {}))
     ([{:keys [env operator-did project-name]
        :or {project-name DEFAULT-PROJECT}}]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
            env (or env {})
            published (pages/write-pages!)
            status0 (deploy-or-refuse {:operator-did (or operator-did "")
                                       :project-name project-name}
                                      :env env)
            runbook (or (:operator-runbook status0) (operator-runbook-facts project-name))
            audit-snap (audit-package-snapshot)
            status (assoc status0
                          :operator-runbook runbook
                          :audit-snapshot audit-snap
                          :package-ready true
                          :package-dir "public"
                          :static-files ["index.html" "facts.edn" "scorecard.md"
                                         "scorecard.edn" "audit-summary.edn"
                                         "deploy-status.edn" "deploy-runbook.edn"
                                         "wrangler.toml" "README.md"]
                          :wrangler-invoked false
                          :cloudflare-api-invoked false
                          :deployed false
                          :live false
                          :cash-usd-micros 0
                          :score-surface []
                          :priority-stack PRIORITY-STACK)
            pub (io/file actor "public")
            wrangler (str "name = \"" project-name "\"\n"
                          "compatibility_date = \"2026-07-17\"\n"
                          "pages_build_output_dir = \".\"\n"
                          "# Generated offline by fuchi.methods.pages-deploy\n"
                          "# Deploy is OUT OF BAND. Scaffold never invokes wrangler.\n"
                          "# cash≡0 live=false no personal scores\n")
            readme (str "# fuchi public surface (static)\n\n"
                        "Generated offline. cash≡0. live=false. No personal scores.\n"
                        "Priority: wellbecoming > mago > ko > present.\n"
                        "Includes displacement→L0 offline enroll + L6 tenure + audit.\n\n"
                        "## Deploy membrane (plan-only)\n\n"
                        "- Default: refused (`" FLAG "` unset).\n"
                        "- Gated plan: flag=1 + operator-did → phase=:gated-deploy-plan,"
                        " still `deployed=false` (no wrangler/API here).\n"
                        "- Actual `wrangler pages deploy public/` is **operator out-of-band**.\n"
                        "- Do not enable live sustenance disbursement from this package.\n"
                        "- land-grant-executed stays 0 until Council-gated live path.\n\n"
                        "### Operator runbook steps\n\n"
                        (apply str (map #(str "1. " % "\n") (:steps runbook)))
                        "\n## Deploy status\n\n"
                        "- phase: " (:phase status) "\n"
                        "- authorized-to-deploy: " (boolean (:authorized-to-deploy status)) "\n"
                        "- package-ready: true\n"
                        "- wrangler-invoked: false\n"
                        "- cloudflare-api-invoked: false\n"
                        "- deployed: false\n"
                        "- live disbursement: never from this package\n"
                        "- last-run land-grant-executed: "
                        (or (:last-run-housing-land-grant-executed audit-snap) 0) "\n")]
        (spit (io/file pub "wrangler.toml") wrangler)
        (spit (io/file pub "deploy-status.edn") (pr-str status))
        (spit (io/file pub "deploy-runbook.edn") (pr-str runbook))
        (spit (io/file pub "README.md") readme)
        (merge published
               {:wrangler (str (io/file pub "wrangler.toml"))
                :deploy-status status
                :deploy-runbook (str (io/file pub "deploy-runbook.edn"))
                :deployed false
                :wrangler-invoked false
                :cloudflare-api-invoked false
                :package-ready true
                :live false
                :cash-usd-micros 0
                :score-surface []
                :priority-stack PRIORITY-STACK})))))
