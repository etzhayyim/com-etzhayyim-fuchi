(ns fuchi.methods.pages-deploy
  "pages_deploy.cljc — Cloudflare Pages deploy membrane for public/ SS surface.

  Default REFUSE (no deploy). Writes a deploy-ready package (wrangler.toml stub +
  status). Even when FUCHI_ALLOW_PAGES_DEPLOY=1 + operator attestation present,
  this scaffold only returns :gated-deploy-plan — it does NOT shell out to wrangler
  or call Cloudflare APIs (side-effect execute remains out of band).

  Public surface only (facts). cash≡0. no scores. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.pages-publish :as pages]
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)
(def FLAG "FUCHI_ALLOW_PAGES_DEPLOY")

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
              :deployed false
              :live false
              :cash-usd-micros 0
              :score-surface []
              :priority-stack PRIORITY-STACK
              :note "Pages deploy default refuse — static package only"}]
     (pp/assert-no-public-scores! out)
     out)))

(defn gated-deploy-plan
  "Authorize a deploy PLAN only. Does not deploy. Requires FUCHI_ALLOW_PAGES_DEPLOY=1
   and non-blank operator-did."
  [{:keys [operator-did project-name]
    :or {project-name "fuchi-public-surface"}}
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
               :authorized-to-deploy true
               :deployed false
               :wrangler-invoked false
               :cloudflare-api-invoked false
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :note "gated deploy plan only — wrangler/API not invoked by scaffold"}]
      (pp/assert-no-public-scores! out)
      out)))

(defn gated-deploy-status
  "Non-raising R1-style status for Pages deploy DESIGN.
   Default env refuses. Never invokes wrangler/Cloudflare API."
  [opts & {:keys [env]}]
  (let [env (or env {})
        operator-did (:operator-did opts)
        project-name (or (:project-name opts) "fuchi-public-surface")]
    (try
      (let [plan (gated-deploy-plan
                  {:operator-did operator-did :project-name project-name}
                  :env env)
            out (assoc plan
                       :admissible true
                       :score-surface []
                       :priority-stack PRIORITY-STACK)]
        (pp/assert-no-public-scores! out)
        out)
      (catch #?(:clj Exception :cljs :default) ex
        (let [st (default-refuse-status env)
              out {:phase :refused
                   :deploy-target "cloudflare-pages"
                   :project-name project-name
                   :admissible false
                   :authorized-to-deploy false
                   :deployed false
                   :wrangler-invoked false
                   :cloudflare-api-invoked false
                   :refusal-reason (or (ex-message ex) (get st "reason"))
                   :gate-admissible (boolean (get st "admissible"))
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :note "Pages deploy gated status — no side-effect; static package only"}]
          (pp/assert-no-public-scores! out)
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

#?(:clj
   (defn write-deploy-package!
     "Refresh public/ via pages-publish + write wrangler.toml stub + deploy-status.edn.
      Never deploys. Returns package map with deployed=false."
     ([]
      (write-deploy-package! {}))
     ([{:keys [env operator-did project-name]
        :or {project-name "fuchi-public-surface"}}]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
            env (or env {})
            published (pages/write-pages!)
            status (deploy-or-refuse {:operator-did (or operator-did "")
                                      :project-name project-name}
                                     :env env)
            pub (io/file actor "public")
            wrangler (str "name = \"" project-name "\"\n"
                          "compatibility_date = \"2026-07-17\"\n"
                          "pages_build_output_dir = \".\"\n"
                          "# Generated offline by fuchi.methods.pages-deploy\n"
                          "# Deploy is OUT OF BAND. Scaffold never invokes wrangler.\n"
                          "# cash≡0 live=false no personal scores\n")
            readme-extra (str "\n## Deploy status\n\n"
                              "- phase: " (:phase status) "\n"
                              "- deployed: false\n"
                              "- live disbursement: never from this package\n")]
        (spit (io/file pub "wrangler.toml") wrangler)
        (spit (io/file pub "deploy-status.edn") (pr-str status))
        (spit (io/file pub "README.md")
              (str "# fuchi public surface (static)\n\n"
                   "Generated offline. cash≡0. live=false. No personal scores.\n"
                   "Priority: wellbecoming > mago > ko > present.\n"
                   "Includes displacement→L0 offline enroll facts when generated.\n\n"
                   "## Deploy membrane\n\n"
                   "- Default: refused (`FUCHI_ALLOW_PAGES_DEPLOY` unset).\n"
                   "- Gated plan: flag=1 + operator-did → still no wrangler invoke here.\n"
                   "- Actual `wrangler pages deploy` is operator out-of-band.\n"
                   "- Do not enable live sustenance disbursement from this package.\n\n"
                   "status phase: " (:phase status) "\n"
                   readme-extra))
        (merge published
               {:wrangler (str (io/file pub "wrangler.toml"))
                :deploy-status status
                :deployed false
                :live false
                :cash-usd-micros 0
                :score-surface []
                :priority-stack PRIORITY-STACK})))))
