(ns fuchi.methods.public-surface-report
  "public_surface_report.cljc — facts-only public surface for covenantal SS (ADR-2607177000).

  Emits markdown + EDN maps of public-person facts from seed. NEVER includes
  priority-rank, share, weight, scores, or percentiles.
  Covers all in-kind rails + dry floor plans + itonami displacement facts.
  Portable .cljc; pure report builders; file write only at #?(:clj) edge optional."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.rail-liquidity-warifu :as liquidity]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.housing-commons-produce-plan :as housprod]
            [fuchi.methods.tooling-okaimono-produce-plan :as tprod]
            [fuchi.methods.compute-murakumo-produce-plan :as cmpprod]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.displacement-surface :as disp]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-gov :as dgov]
            [fuchi.methods.displacement-scorecard :as dsc]
            [fuchi.methods.ss-offline-path :as ss-path]
            #?(:clj [fuchi.methods.pipeline-audit-ledger :as audit])
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- pages-deploy-refuse-status
  "Default Pages deploy membrane status (no require of pages-deploy — cycle-safe).
   Includes plan-only design facts. Does not deploy. cash≡0. live=false."
  []
  (let [runbook {:flag "FUCHI_ALLOW_PAGES_DEPLOY"
                 :required-for-gated-plan ["FUCHI_ALLOW_PAGES_DEPLOY=1" "operator-did non-blank"]
                 :scaffold-invokes-wrangler false
                 :scaffold-invokes-cloudflare-api false
                 :side-effect-execute "out-of-band only"
                 :live-disbursement false
                 :deployed false
                 :live false
                 :cash-usd-micros 0
                 :score-surface []
                 :priority-stack PRIORITY-STACK
                 :steps ["write-deploy-package! → refresh public/ static facts"
                         "review index.html / facts.edn (no personal scores)"
                         "optional gated plan: FUCHI_ALLOW_PAGES_DEPLOY=1 + operator-did"
                         "out-of-band: wrangler pages deploy public/"
                         "never enable live sustenance from this package"]
                 :note "plan-only membrane; actual deploy is operator out-of-band"}
        out {:phase :refused
             :deploy-target "cloudflare-pages"
             :admissible false
             :authorized-to-deploy false
             :package-ready true
             :operator-flag "FUCHI_ALLOW_PAGES_DEPLOY"
             :refusal-reason "missing operator process flag 'FUCHI_ALLOW_PAGES_DEPLOY'"
             :wrangler-invoked false
             :cloudflare-api-invoked false
             :operator-runbook runbook
             :deployed false
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "Pages deploy default refuse — static package only; plan-only membrane"}]
    (pp/assert-no-public-scores! (dissoc out :operator-runbook))
    (pp/assert-no-public-scores! runbook)
    out))

(defn- last-seg [s]
  (last (str/split (str s) #":")))

(defn- line* [e]
  (-> (str (or (get e ":envelope/line") (get e :envelope/line) ""))
      (str/replace #"^:" "")
      (str/split #"/")
      last
      str/lower-case))

(defn- imp-for [envs line]
  (reduce + 0 (map #(long (or (get % ":envelope/imputed-usd-micros-yr")
                              (get % :envelope/imputed-usd-micros-yr)
                              0))
                   (filter #(= line (line* %)) envs))))

(defn person-fact-row
  "One public fact row (map). Strips scores."
  [surf]
  (let [row {:did (:did surf)
             :public-person? (boolean (:public-person? surf))
             :covenant (:covenant surf)
             :stage (or (:stage surf) "L2")
             :rails (vec (or (:rails surf) []))
             :imputed-fact (or (:imputed-fact surf) 0)
             :disclosure-status (or (:disclosure-status surf)
                                    (get-in surf [:disclosure-gate :action]))
             :hold-reason-code (:hold-reason-code surf)
             :priority-stack PRIORITY-STACK
             :score-surface []}]
    (pp/assert-no-public-scores! row)
    row))

(defn seed-public-facts
  "All public-person fact rows from a seed graph."
  [seed]
  (mapv person-fact-row (pp/persons-from-seed seed)))

(defn- phase-of [pkg]
  (when pkg (name (:phase pkg))))

(defn- floor-facts
  "Extract non-score floor fields from a dry produce plan (facts only)."
  [plan]
  (when (and plan (not= :refused (:phase plan)))
    (cond-> {:phase (name (:phase plan))
             :produce-executed (boolean (:produce-executed plan false))
             :live false
             :cash-usd-micros 0
             :score-surface []}
      (:kcal-floor-yr plan) (assoc :kcal-floor-yr (:kcal-floor-yr plan))
      (:kwh-floor-yr plan) (assoc :kwh-floor-yr (:kwh-floor-yr plan))
      (:care-hours-floor-yr plan) (assoc :care-hours-floor-yr (:care-hours-floor-yr plan))
      (:housing-months-floor-yr plan) (assoc :housing-months-floor-yr (:housing-months-floor-yr plan))
      (:tool-units-floor-yr plan) (assoc :tool-units-floor-yr (:tool-units-floor-yr plan))
      (:gpu-hours-floor-yr plan) (assoc :gpu-hours-floor-yr (:gpu-hours-floor-yr plan)))))

(defn- safe-plan [plan-fn pkg]
  (when (and pkg (not= :refused (:phase pkg)))
    (try (plan-fn pkg) (catch Exception _ nil))))

(defn inkind-rail-packages
  "R1 dry packages + dry floor plans for all in-kind rails (facts only; no live execute)."
  [seed]
  (let [surfs (pp/persons-from-seed seed)
        recs (get seed ":maintainer/batch" [])
        env-of (fn [did]
                 (filterv #(= (get % ":envelope/maintainer") did)
                          (get seed ":envelope/batch" [])))]
    (vec
     (for [surf surfs
           :let [did (:did surf)
                 rec (first (filter #(= (get % ":maintainer/did") did) recs))
                 envs (env-of did)
                 food-imp (imp-for envs "food")
                 energy-imp (imp-for envs "energy")
                 care-imp (imp-for envs "care")
                 housing-imp (imp-for envs "housing")
                 tooling-imp (imp-for envs "tooling")
                 compute-imp (imp-for envs "compute")
                 liquidity-imp (imp-for envs "liquidity")
                 drec (pp/disclosure-for-did seed did)
                 person (pp/persons-from-seed-row rec envs drec)
                 hm (dh/from-seed-person person)
                 food-pkg (when (pos? food-imp)
                            (mitsuho/r1-dry-package
                             {:alloc-id (str "food-" (last-seg did)) :subject-did did
                              :imputed-usd-micros-yr food-imp :person person :hold-machine hm}))
                 energy-pkg (when (pos? energy-imp)
                              (hikari/r1-dry-package
                               {:alloc-id (str "energy-" (last-seg did)) :subject-did did
                                :imputed-usd-micros-yr energy-imp :person person :hold-machine hm}))
                 care-pkg (when (pos? care-imp)
                            (care/r1-dry-package
                             {:alloc-id (str "care-" (last-seg did)) :subject-did did
                              :imputed-usd-micros-yr care-imp :person person :hold-machine hm}))
                 housing-pkg (when (pos? housing-imp)
                               (housing/r1-dry-package
                                {:alloc-id (str "housing-" (last-seg did)) :subject-did did
                                 :imputed-usd-micros-yr housing-imp :person person :hold-machine hm}))
                 tooling-pkg (when (pos? tooling-imp)
                               (tooling/r1-dry-package
                                {:alloc-id (str "tooling-" (last-seg did)) :subject-did did
                                 :imputed-usd-micros-yr tooling-imp :person person :hold-machine hm}))
                 compute-pkg (when (pos? compute-imp)
                               (compute/r1-dry-package
                                {:alloc-id (str "compute-" (last-seg did)) :subject-did did
                                 :imputed-usd-micros-yr compute-imp :person person :hold-machine hm}))
                 liquidity-pkg (when (pos? liquidity-imp)
                                 (liquidity/r1-dry-package
                                  {:alloc-id (str "liquidity-" (last-seg did)) :subject-did did
                                   :imputed-usd-micros-yr liquidity-imp :person person :hold-machine hm}))]
           :when (or (pos? food-imp) (pos? energy-imp) (pos? care-imp) (pos? housing-imp)
                     (pos? tooling-imp) (pos? compute-imp) (pos? liquidity-imp))]
       (let [row {:did did
                  :disclosure-state (name (:state hm))
                  :food food-pkg
                  :food-floor (floor-facts (safe-plan mprod/plan-from-r1 food-pkg))
                  :energy energy-pkg
                  :energy-floor (floor-facts (safe-plan hprod/plan-from-r1 energy-pkg))
                  :care care-pkg
                  :care-floor (floor-facts (safe-plan cprod/plan-from-r1 care-pkg))
                  :housing housing-pkg
                  :housing-floor (floor-facts (safe-plan housprod/plan-from-r1 housing-pkg))
                  :tooling tooling-pkg
                  :tooling-floor (floor-facts (safe-plan tprod/plan-from-r1 tooling-pkg))
                  :compute compute-pkg
                  :compute-floor (floor-facts (safe-plan cmpprod/plan-from-r1 compute-pkg))
                  :liquidity liquidity-pkg
                  :cash-usd-micros 0
                  :live false
                  :score-surface []
                  :priority-stack PRIORITY-STACK}]
         (pp/assert-no-public-scores! row)
         row)))))

(defn food-energy-packages
  "Backward-compatible alias: food+energy subset of inkind-rail-packages."
  [seed]
  (mapv (fn [r]
          (select-keys r [:did :disclosure-state :food :energy]))
        (inkind-rail-packages seed)))

(defn l0-demo-fact
  "Optional L0 enroll demo fact for a DID (offline)."
  [subject-did]
  (let [e (l0/enroll {:subject-did subject-did
                      :vow-text "demo L0 for public surface report"
                      :member-signature (str "sig-" (last-seg subject-did))
                      :covenant "outreach"})]
    {:did subject-did
     :stage "L0"
     :public-person? (get-in e [:public-person :public-person?])
     :token-stub (get-in e [:vow :token-id])
     :kotoba-cid-stub (get-in e [:vow :kotoba-cid])
     :cash-usd-micros 0
     :live false
     :score-surface []
     :priority-stack PRIORITY-STACK}))

(defn ss-priority-path-public-fact
  "Facts-only projection of ss_offline_path priority (1)(2)(3) demo.
   L0 enroll + disclosure continuity + all rails gated-live DESIGN refuse + R2 refuse.
   No personal scores. cash≡0. live=false."
  ([]
   (ss-priority-path-public-fact
    "did:web:etzhayyim.com:member:ss-priority-demo"))
  ([subject-did]
   (let [path (ss-path/run-food-path
               {:subject-did subject-did
                :food-imputed-usd-micros-yr 2000000000
                :energy-imputed-usd-micros-yr 1500000000
                :care-imputed-usd-micros-yr 1000000000
                :housing-imputed-usd-micros-yr 12000000000
                :tooling-imputed-usd-micros-yr 500000000
                :compute-imputed-usd-micros-yr 800000000
                :liquidity-imputed-usd-micros-yr 1500000000
                :include-disclosure-stress true})
         s (or (:priority-path-summary path) {})
         out {:path (:path path)
              :did subject-did
              :l0-stage (:l0-stage s)
              :l0-published (boolean (:l0-published s))
              :l0-token-stub (:l0-token-stub s)
              :ladder-phase (:ladder-phase s)
              :ladder-from (:ladder-from s)
              :ladder-to (:ladder-to s)
              :ladder-target (:ladder-target s)
              :ladder-steps (or (:ladder-steps s) 0)
              :ladder-rails-hint (vec (or (:ladder-rails-hint s) []))
              :ladder-rails-hint-first (:ladder-rails-hint-first s)
              :ladder-multi-gen-fact (:ladder-multi-gen-fact s)
              :ladder-published false
              :held-stress-ladder-refused (boolean (:held-stress-ladder-refused s))
              :disclosure-state (:disclosure-state s)
              :entitlements-may-flow? (boolean (:entitlements-may-flow? s))
              :continuity-action (when-let [a (:continuity-action s)]
                                   (if (keyword? a) (name a) (str a)))
              :held-stress-held? (boolean (:held-stress-held? s))
              :held-stress-food-phase (:held-stress-food-phase s)
              :mitsuho-r1-phase (:mitsuho-r1-phase s)
              :mitsuho-gated-admissible (boolean (:mitsuho-gated-admissible s))
              :mitsuho-produce-executed false
              :hikari-r1-phase (:hikari-r1-phase s)
              :hikari-gated-admissible (boolean (:hikari-gated-admissible s))
              :hikari-generate-executed false
              :care-r1-phase (:care-r1-phase s)
              :care-gated-admissible (boolean (:care-gated-admissible s))
              :housing-r1-phase (:housing-r1-phase s)
              :housing-gated-admissible (boolean (:housing-gated-admissible s))
              :housing-land-grant-executed false
              :tooling-r1-phase (:tooling-r1-phase s)
              :tooling-gated-admissible (boolean (:tooling-gated-admissible s))
              :compute-r1-phase (:compute-r1-phase s)
              :compute-gated-admissible (boolean (:compute-gated-admissible s))
              :liquidity-r1-phase (:liquidity-r1-phase s)
              :liquidity-gated-admissible (boolean (:liquidity-gated-admissible s))
              :liquidity-loan-executed false
              :liquidity-member-principal (boolean (:liquidity-member-principal s true))
              :liquidity-cash-usd-micros 0
              :rails-gated-count (or (:rails-gated-count s) 0)
              :rails-gated-admissible-count (or (:rails-gated-admissible-count s) 0)
              :all-rails-gated-refused (boolean (:all-rails-gated-refused s))
              :r2-status-count (or (:r2-status-count s) 0)
              :r2-executed-count (or (:r2-executed-count s) 0)
              :all-r2-not-executed (boolean (:all-r2-not-executed s true))
              :rail-gated (or (:rail-gated s) {})
              :r2-food-phase (:r2-food-phase s)
              :r2-food-executed (boolean (:r2-food-executed s))
              :r2-energy-phase (:r2-energy-phase s)
              :r2-energy-executed (boolean (:r2-energy-executed s))
              :public-person? (boolean (get-in path [:public-person :public-person?]))
              :live false
              :cash-usd-micros 0
              :score-surface []
              :priority-stack PRIORITY-STACK
              :note "priority path offline — L0 + disclosure + all rails gated refuse + R2 refuse"}]
     (pp/assert-no-public-scores! (dissoc out :rail-gated))
     out)))

(defn displacement-l0-public-summary
  "Facts-only projection of displacement→L0/L4 batch (no subject scores).
   Includes disclosure open/held and mitsuho/hikari R1→gated refuse facts when present."
  [batch]
  (let [subjects (mapcat :subjects (or (:packages batch) []))
        pkgs (mapv
              (fn [p]
                (let [subs (:subjects p)
                      ten-subs (or (:tenure-subjects p) [])
                      stages (frequencies (map :stage subs))
                      c (:couple p)
                      tc (:tenure-couple p)
                      open-n (count (filter #(or (true? (:entitlements-may-flow? %))
                                                 (= :open (:disclosure-state %))
                                                 (= :open (get-in % [:disclosure-hold :state])))
                                            subs))
                      held-n (count (filter #(or (true? (:disclosure-held? %))
                                                 (= :held (:disclosure-state %))
                                                 (false? (:entitlements-may-flow? %)))
                                            subs))
                      c-pre (:couple-pre-gov p)
                      row {:cohort-id (:cohort-id p)
                           :displacing-actor (:displacing-actor p)
                           :phase (name (:phase p))
                           :subject-count (count subs)
                           :stages stages
                           ;; flowable-first committed (housing held under Council excluded)
                           :committed-usd-micros-yr (or (:committed-usd-micros-yr c) 0)
                           :committed-full-usd-micros-yr
                           (or (:committed-full-usd-micros-yr c)
                               (:committed-usd-micros-yr c-pre)
                               (:committed-usd-micros-yr c)
                               0)
                           :earmark-usd-micros-yr (or (:earmark-usd-micros-yr c)
                                                      (get-in p [:earmark :earmark-usd-micros-yr])
                                                      0)
                           :headroom-usd-micros-yr (or (:headroom-usd-micros-yr c) 0)
                           :gov-flowable-usd-micros
                           (or (:gov-flowable-committed-usd-micros p) 0)
                           :gov-post-ratify-usd-micros
                           (or (:gov-post-ratify-committed-usd-micros p) 0)
                           ;; L6 tenure path (when batch includes tenure + gov package)
                           :tenure-phase (when (:tenure-phase p) (name (:tenure-phase p)))
                           :tenure-subjects (count ten-subs)
                           :tenure-g2 (boolean (and tc (true? (:admissible tc))))
                           :tenure-committed-usd-micros-yr
                           (or (:committed-usd-micros-yr tc) 0)
                           :tenure-committed-full-usd-micros-yr
                           (or (:committed-full-usd-micros-yr tc)
                               (get-in p [:tenure-couple-pre-gov :committed-usd-micros-yr])
                               0)
                           :tenure-gov-flowable-usd-micros
                           (or (:tenure-gov-flowable-committed-usd-micros p) 0)
                           :tenure-gov-post-ratify-usd-micros
                           (or (:tenure-gov-post-ratify-committed-usd-micros p) 0)
                           ;; refused / missing couple → not G2-admissible (no free-riding)
                           :g2-admissible (boolean (and c (true? (:admissible c))))
                           :funded (boolean (or (:funded c)
                                               (get-in p [:earmark :funded])
                                               (get-in p [:couple :funded])))
                           :disclosure-open open-n
                           :disclosure-held held-n
                           :mitsuho-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:food-package :phase])) subs))
                           :mitsuho-gated-refused
                           (count (filter #(false? (get-in % [:food-gated-live-status :admissible]))
                                          (filter :food-gated-live-status subs)))
                           :hikari-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:energy-package :phase])) subs))
                           :hikari-gated-refused
                           (count (filter #(false? (get-in % [:energy-gated-live-status :admissible]))
                                          (filter :energy-gated-live-status subs)))
                           :care-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:care-package :phase])) subs))
                           :care-gated-refused
                           (count (filter #(false? (get-in % [:care-gated-live-status :admissible]))
                                          (filter :care-gated-live-status subs)))
                           :housing-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:housing-package :phase])) subs))
                           :housing-gated-refused
                           (count (filter #(false? (get-in % [:housing-gated-live-status :admissible]))
                                          (filter :housing-gated-live-status subs)))
                           :housing-land-grant-executed
                           (count (filter #(true? (get-in % [:housing-gated-live-status :land-grant-executed]))
                                          (filter :housing-gated-live-status subs)))
                           :tooling-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:tooling-package :phase])) subs))
                           :tooling-gated-refused
                           (count (filter #(false? (get-in % [:tooling-gated-live-status :admissible]))
                                          (filter :tooling-gated-live-status subs)))
                           :compute-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:compute-package :phase])) subs))
                           :compute-gated-refused
                           (count (filter #(false? (get-in % [:compute-gated-live-status :admissible]))
                                          (filter :compute-gated-live-status subs)))
                           :liquidity-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:liquidity-package :phase])) subs))
                           :liquidity-gated-refused
                           (count (filter #(false? (get-in % [:liquidity-gated-live-status :admissible]))
                                          (filter :liquidity-gated-live-status subs)))
                           :liquidity-member-principal
                           (count (filter #(true? (get-in % [:liquidity-package :member-principal]))
                                          subs))
                           :liquidity-loan-executed
                           (count (filter #(true? (get-in % [:liquidity-gated-live-status :loan-executed]))
                                          (filter :liquidity-gated-live-status subs)))
                           :liquidity-cash-usd-micros
                           (reduce + 0 (map #(or (get-in % [:liquidity-package :cash-usd-micros]) 0) subs))
                           :refusal-reason (:refusal-reason p)
                           :cash-usd-micros 0
                           :live false
                           :score-surface []
                           :priority-stack PRIORITY-STACK}]
                  (pp/assert-no-public-scores! row)
                  row))
              (or (:packages batch) []))]
    {:admissible-cohorts (or (:admissible-cohorts batch) 0)
     :refused-cohorts (or (:refused-cohorts batch) 0)
     :enrolled-subjects (or (:enrolled-subjects batch) 0)
     :stage-counts (or (:stage-counts batch) (frequencies (map :stage subjects)))
     ;; prefer per-package flowable/full sums (post-G7) over bare batch totals
     :committed-usd-micros-yr
     (let [s (reduce + 0 (map #(or (:committed-usd-micros-yr %) 0) pkgs))]
       (if (pos? s) s (or (:committed-usd-micros-yr batch) 0)))
     :disclosure-open (reduce + 0 (map #(or (:disclosure-open %) 0) pkgs))
     :disclosure-held (reduce + 0 (map #(or (:disclosure-held %) 0) pkgs))
     :mitsuho-r1-dry (reduce + 0 (map #(or (:mitsuho-r1-dry %) 0) pkgs))
     :mitsuho-gated-refused (reduce + 0 (map #(or (:mitsuho-gated-refused %) 0) pkgs))
     :hikari-r1-dry (reduce + 0 (map #(or (:hikari-r1-dry %) 0) pkgs))
     :hikari-gated-refused (reduce + 0 (map #(or (:hikari-gated-refused %) 0) pkgs))
     :care-r1-dry (reduce + 0 (map #(or (:care-r1-dry %) 0) pkgs))
     :care-gated-refused (reduce + 0 (map #(or (:care-gated-refused %) 0) pkgs))
     :housing-r1-dry (reduce + 0 (map #(or (:housing-r1-dry %) 0) pkgs))
     :housing-gated-refused (reduce + 0 (map #(or (:housing-gated-refused %) 0) pkgs))
     :housing-land-grant-executed
     (reduce + 0 (map #(or (:housing-land-grant-executed %) 0) pkgs))
     :tooling-r1-dry (reduce + 0 (map #(or (:tooling-r1-dry %) 0) pkgs))
     :tooling-gated-refused (reduce + 0 (map #(or (:tooling-gated-refused %) 0) pkgs))
     :compute-r1-dry (reduce + 0 (map #(or (:compute-r1-dry %) 0) pkgs))
     :compute-gated-refused (reduce + 0 (map #(or (:compute-gated-refused %) 0) pkgs))
     :liquidity-r1-dry (reduce + 0 (map #(or (:liquidity-r1-dry %) 0) pkgs))
     :liquidity-gated-refused (reduce + 0 (map #(or (:liquidity-gated-refused %) 0) pkgs))
     :liquidity-member-principal
     (reduce + 0 (map #(or (:liquidity-member-principal %) 0) pkgs))
     :liquidity-loan-executed
     (reduce + 0 (map #(or (:liquidity-loan-executed %) 0) pkgs))
     :liquidity-cash-usd-micros
     (reduce + 0 (map #(or (:liquidity-cash-usd-micros %) 0) pkgs))
     :earmark-usd-micros-yr
     (reduce + 0 (map #(or (:earmark-usd-micros-yr %) 0) pkgs))
     :headroom-usd-micros-yr
     (reduce + 0 (map #(or (:headroom-usd-micros-yr %) 0) pkgs))
     :committed-full-usd-micros-yr
     (reduce + 0 (map #(or (:committed-full-usd-micros-yr %) 0) pkgs))
     :gov-flowable-usd-micros
     (reduce + 0 (map #(or (:gov-flowable-usd-micros %) 0) pkgs))
     :gov-post-ratify-usd-micros
     (reduce + 0 (map #(or (:gov-post-ratify-usd-micros %) 0) pkgs))
     :tenure-subjects
     (reduce + 0 (map #(or (:tenure-subjects %) 0) pkgs))
     :tenure-g2-cohorts
     (count (filter :tenure-g2 pkgs))
     :tenure-committed-usd-micros-yr
     (reduce + 0 (map #(or (:tenure-committed-usd-micros-yr %) 0) pkgs))
     :tenure-committed-full-usd-micros-yr
     (reduce + 0 (map #(or (:tenure-committed-full-usd-micros-yr %) 0) pkgs))
     :tenure-gov-flowable-usd-micros
     (reduce + 0 (map #(or (:tenure-gov-flowable-usd-micros %) 0) pkgs))
     :tenure-gov-post-ratify-usd-micros
     (reduce + 0 (map #(or (:tenure-gov-post-ratify-usd-micros %) 0) pkgs))
     :g2-admissible-cohorts
     (count (filter :g2-admissible pkgs))
     :packages pkgs
     :cash-usd-micros 0
     :live false
     :score-surface []
     :priority-stack PRIORITY-STACK}))

(defn report-edn
  "Full facts-only report structure."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0
                  include-scorecard include-audit include-ss-priority-path]
           :or {include-displacement-l0 true include-scorecard true include-audit true
                include-ss-priority-path true}}]
  (let [facts (seed-public-facts seed)
        rails (inkind-rail-packages seed)
        drows (disp/public-displacement-facts seed)
        itonami-rows (when include-itonami
                       #?(:clj
                          (try
                            (itonami/public-facts-from-itonami
                             (itonami/load-itonami-seed-file) seed)
                            (catch Exception _ []))
                          :cljs []))
        dl0-batch #?(:clj
                     (when include-displacement-l0
                       (try
                         ;; L0→L6 tenure + G7 package so public L0 shows flowable-first committed
                         (let [batch0 (dl0/run-default-seed :max-slots 2 :climb-steps 4)
                               actor (or (System/getenv "FUCHI_ACTOR_DIR")
                                         (-> *file* io/file .getParentFile .getParentFile
                                             .getCanonicalPath))
                               seed (edn/load-edn
                                     (io/file actor "data" "itonami-displacement-events.edn"))
                               evs (itonami/load-itonami-batch seed)
                               with-ten (if (seq evs)
                                          (ten/run-batch-with-tenure batch0 evs
                                                                    :target-stage "L6")
                                          batch0)]
                           (dgov/package-batch with-ten))
                         (catch Exception _ nil)))
                     :cljs nil)
        dl0-sum (when dl0-batch (displacement-l0-public-summary dl0-batch))
        scard (when include-scorecard
                #?(:clj
                   (try
                     (if dl0-batch (dsc/build dl0-batch) (dsc/build))
                     (catch Exception _ nil))
                   :cljs nil))
        audit-sum (when include-audit
                    #?(:clj
                       (try (audit/summary)
                            (catch Exception _ {:runs 0 :live false :cash-usd-micros 0
                                                :score-surface []
                                                :priority-stack PRIORITY-STACK
                                                :any-land-grant-executed? false
                                                :all-runs-live-refused true}))
                       :cljs nil))
        body {:report/id "fuchi.public-surface"
              :report/adr ["2607177000" "2606032130"]
              :report/priority-stack PRIORITY-STACK
              :report/score-surface []
              :report/cash-usd-micros 0
              :report/live false
              :report/public-persons facts
              :report/rail-packages rails
              :report/displacement drows
              :report/displacement-summary (disp/summary drows)
              :report/itonami-displacement (or itonami-rows [])
              :report/displacement-l0 (or dl0-sum {})
              :report/displacement-scorecard (or scard {})
              :report/pipeline-audit (or audit-sum {})
              :report/pages-deploy-status (pages-deploy-refuse-status)
              :report/ss-priority-path
              (when include-ss-priority-path
                (try (ss-priority-path-public-fact)
                     (catch #?(:clj Exception :cljs :default) _
                       {:path "ss-offline-inkind-rails"
                        :error "ss-priority-path unavailable"
                        :live false :cash-usd-micros 0 :score-surface []
                        :priority-stack PRIORITY-STACK})))
              :report/l0-demo (when include-l0-demo
                                (l0-demo-fact "did:web:etzhayyim.com:member:lot"))}]
    (doseq [f facts] (pp/assert-no-public-scores! f))
    (doseq [d drows] (pp/assert-no-public-scores! d))
    (when-let [sp (:report/ss-priority-path body)]
      (pp/assert-no-public-scores! (dissoc sp :error)))
    body))

(defn report-md
  "Markdown facts-only public surface (no ranks/scores)."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0]
           :or {include-displacement-l0 true}}]
  (let [body (report-edn seed :include-l0-demo include-l0-demo :include-itonami include-itonami
                         :include-displacement-l0 include-displacement-l0)
        lines (transient
               ["# fuchi — public surface (facts only)\n"
                (str "Priority: wellbecoming > mago(孫) > ko(子) > present. "
                     "Scores/ranks unrepresentable. cash≡0. live=false.\n")
                "## Public persons\n"
                "| did | covenant | public? | disclosure | rails | imputed |\n"
                "|---|---|---|---|---|---|\n"])]
    (doseq [f (:report/public-persons body)]
      (conj! lines
             (str "| " (last-seg (:did f)) " | " (:covenant f) " | "
                  (:public-person? f) " | " (:disclosure-status f) " | "
                  (str/join "," (:rails f)) " | " (:imputed-fact f) " |\n")))
    (conj! lines "\n## Rail packages (R1 dry + floor plans; no live execute)\n")
    (conj! lines "| did | food | energy | care | housing | tooling | compute | liquidity |\n")
    (conj! lines "|---|---|---|---|---|---|---|---|\n")
    (doseq [r (:report/rail-packages body)]
      (conj! lines
             (str "| " (last-seg (:did r)) " | "
                  (or (phase-of (:food r)) "—") " | "
                  (or (phase-of (:energy r)) "—") " | "
                  (or (phase-of (:care r)) "—") " | "
                  (or (phase-of (:housing r)) "—") " | "
                  (or (phase-of (:tooling r)) "—") " | "
                  (or (phase-of (:compute r)) "—") " | "
                  (or (phase-of (:liquidity r)) "—") " |\n")))
    (conj! lines "\n## Dry floor facts (produce-executed=false)\n")
    (conj! lines "| did | kcal | kWh | care-h | housing-mo | tools | GPU-h |\n")
    (conj! lines "|---|---|---|---|---|---|---|\n")
    (doseq [r (:report/rail-packages body)]
      (conj! lines
             (str "| " (last-seg (:did r)) " | "
                  (or (get-in r [:food-floor :kcal-floor-yr]) "—") " | "
                  (or (get-in r [:energy-floor :kwh-floor-yr]) "—") " | "
                  (or (get-in r [:care-floor :care-hours-floor-yr]) "—") " | "
                  (or (get-in r [:housing-floor :housing-months-floor-yr]) "—") " | "
                  (or (get-in r [:tooling-floor :tool-units-floor-yr]) "—") " | "
                  (or (get-in r [:compute-floor :gpu-hours-floor-yr]) "—") " |\n")))
    (conj! lines "\n## Displacement → earmark (itonami/robotics coupling facts)\n")
    (conj! lines "| actor | cohort | displaced | funded | admissible | earmark USD micros |\n")
    (conj! lines "|---|---|---|---|---|---|\n")
    (doseq [d (:report/displacement body)]
      (conj! lines
             (str "| " (:displacing-actor d) " | " (:cohort-id d) " | "
                  (:displaced-count d) " | " (:funded d) " | " (:admissible d) " | "
                  (:earmark-usd-micros-yr d) " |\n")))
    (when (seq (:report/itonami-displacement body))
      (conj! lines "\n## itonami surplus bridge (offline seed)\n")
      (conj! lines "| actor | cohort | displaced | funded | admissible |\n|---|---|---|---|---|\n")
      (doseq [d (:report/itonami-displacement body)]
        (conj! lines
               (str "| " (:displacing-actor d) " | " (:cohort-id d) " | "
                    (:displaced-count d) " | " (:funded d) " | " (:admissible d) " |\n"))))
    (let [s (:report/displacement-summary body)]
      (conj! lines (str "\nSummary: events=" (:displacement-events s)
                        " admissible=" (:funded-admissible s)
                        " refused=" (:refused s)
                        " total-displaced=" (:total-displaced s) "\n")))
    (when-let [dl0 (:report/displacement-l0 body)]
      (when (seq (:packages dl0))
        (conj! lines "\n## Displacement → L0→L4 enroll (offline; G2 gated)\n")
        (conj! lines (str "admissible-cohorts=" (:admissible-cohorts dl0)
                          " refused-cohorts=" (:refused-cohorts dl0)
                          " enrolled-subjects=" (:enrolled-subjects dl0)
                          " stages=" (pr-str (:stage-counts dl0)) "\n"))
        (conj! lines (str "disclosure-open=" (or (:disclosure-open dl0) 0)
                          " disclosure-held=" (or (:disclosure-held dl0) 0)
                          " g2-admissible-cohorts=" (or (:g2-admissible-cohorts dl0) 0) "\n"))
        (conj! lines (str "earmark-total=" (or (:earmark-usd-micros-yr dl0) 0)
                          " committed-flowable-total=" (or (:committed-usd-micros-yr dl0) 0)
                          " committed-full-total=" (or (:committed-full-usd-micros-yr dl0) 0)
                          " headroom-total=" (or (:headroom-usd-micros-yr dl0) 0) "\n"))
        (conj! lines (str "gov-flowable-total=" (or (:gov-flowable-usd-micros dl0) 0)
                          " gov-post-ratify-total=" (or (:gov-post-ratify-usd-micros dl0) 0) "\n"))
        (conj! lines (str "tenure-subjects=" (or (:tenure-subjects dl0) 0)
                          " tenure-g2-cohorts=" (or (:tenure-g2-cohorts dl0) 0)
                          " tenure-committed-flow=" (or (:tenure-committed-usd-micros-yr dl0) 0)
                          " tenure-committed-full=" (or (:tenure-committed-full-usd-micros-yr dl0) 0) "\n"))
        (conj! lines (str "tenure-gov-flowable=" (or (:tenure-gov-flowable-usd-micros dl0) 0)
                          " tenure-gov-post-ratify=" (or (:tenure-gov-post-ratify-usd-micros dl0) 0) "\n"))
        (conj! lines (str "land-grant-executed-total="
                          (or (:housing-land-grant-executed dl0) 0)
                          " (post-ratify plan keeps land-grant=false; Council-gated)\n"))
        (conj! lines (str "| actor | cohort | phase | n | g2 | funded | earmark | disc-o/h "
                          "| L4-flow | L4-full | L4-post | ten-n | ten-flow | ten-post "
                          "| land-grant | headroom |\n"
                          "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n"))
        (doseq [p (:packages dl0)]
          (conj! lines
                 (str "| " (:displacing-actor p) " | " (:cohort-id p) " | "
                      (:phase p) " | " (:subject-count p) " | "
                      (:g2-admissible p) " | "
                      (boolean (:funded p)) " | "
                      (or (:earmark-usd-micros-yr p) 0) " | "
                      (or (:disclosure-open p) 0) "/"
                      (or (:disclosure-held p) 0) " | "
                      (:committed-usd-micros-yr p) " | "
                      (or (:committed-full-usd-micros-yr p) 0) " | "
                      (or (:gov-post-ratify-usd-micros p) 0) " | "
                      (or (:tenure-subjects p) 0) " | "
                      (or (:tenure-committed-usd-micros-yr p) 0) " | "
                      (or (:tenure-gov-post-ratify-usd-micros p) 0) " | "
                      (or (:housing-land-grant-executed p) 0) " | "
                      (:headroom-usd-micros-yr p) " |\n"))))
    (when-let [sc (:report/displacement-scorecard body)]
      (when (seq sc)
        (conj! lines "\n## Displacement SS scorecard (offline)\n")
        (conj! lines (str "- all-live-refused: " (:scorecard/all-live-refused sc) "\n"))
        (conj! lines (str "- booked-entries: " (:scorecard/booked-entries sc) "\n"))
        (conj! lines (str "- committed (flowable-first): "
                         (:scorecard/committed-usd-micros-yr sc) "\n"))
        (conj! lines (str "- headroom: " (:scorecard/headroom-usd-micros-yr sc) "\n"))
        (conj! lines (str "- tenure-subjects (L6): " (:scorecard/tenure-subjects sc) "\n"))
        (conj! lines (str "- tenure-stages: " (pr-str (:scorecard/tenure-stage-counts sc)) "\n"))
        (conj! lines (str "- gov-routes: " (pr-str (:scorecard/gov-route-counts sc)) "\n"))
        (conj! lines "  (housing held for council-lv7; multi-gen substrate may dry-flow)\n")
        (conj! lines (str "- gov-flowable / gov-post-ratify: "
                         (or (:scorecard/gov-flowable-committed-usd-micros sc) 0) "/"
                         (or (:scorecard/gov-post-ratify-committed-usd-micros sc) 0) "\n"))
        (conj! lines (str "- tenure-gov-flowable / tenure-gov-post-ratify: "
                         (or (:scorecard/tenure-gov-flowable-committed-usd-micros sc) 0) "/"
                         (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc) 0) "\n"))
        (conj! lines (str "- land-grant-executed (post-ratify plan still false): "
                         (or (:scorecard/housing-land-grant-executed sc) 0) "\n"))
        (conj! lines (str "- R2 execute statuses/refused/executed: "
                         (or (:scorecard/r2-status-count sc) 0) "/"
                         (or (:scorecard/r2-refused sc) 0) "/"
                         (or (:scorecard/r2-executed sc) 0)
                         " all-r2-not-executed="
                         (boolean (:scorecard/all-r2-not-executed sc true)) "\n"))
        (conj! lines (str "- L4 disclosure open/held: "
                         (or (:scorecard/l4-disclosure-open sc) 0) "/"
                         (or (:scorecard/l4-disclosure-held sc) 0) "\n"))
        (conj! lines (str "- mitsuho food R1/gated/produce: "
                         (or (:scorecard/mitsuho-r1-dry sc) 0) "/"
                         (or (:scorecard/mitsuho-gated-refused sc) 0) "/"
                         (or (:scorecard/mitsuho-produce-executed sc) 0) "\n"))
        (conj! lines (str "- hikari energy R1/gated/generate: "
                         (or (:scorecard/hikari-r1-dry sc) 0) "/"
                         (or (:scorecard/hikari-gated-refused sc) 0) "/"
                         (or (:scorecard/hikari-generate-executed sc) 0) "\n"))
        (conj! lines (str "- care-iyashi R1/gated/delivery: "
                         (or (:scorecard/care-r1-dry sc) 0) "/"
                         (or (:scorecard/care-gated-refused sc) 0) "/"
                         (or (:scorecard/care-delivery-executed sc) 0) "\n"))
        (conj! lines (str "- housing-commons R1/gated/land-grant: "
                         (or (:scorecard/housing-r1-dry sc) 0) "/"
                         (or (:scorecard/housing-gated-refused sc) 0) "/"
                         (or (:scorecard/housing-land-grant-executed sc) 0) "\n"))
        (conj! lines (str "- housing council-held: "
                         (or (:scorecard/housing-council-held sc) 0) "\n"))
        (conj! lines (str "- tooling-okaimono R1/gated/fulfill: "
                         (or (:scorecard/tooling-r1-dry sc) 0) "/"
                         (or (:scorecard/tooling-gated-refused sc) 0) "/"
                         (or (:scorecard/tooling-fulfillment-executed sc) 0) "\n"))
        (conj! lines (str "- compute-murakumo R1/gated/quota: "
                         (or (:scorecard/compute-r1-dry sc) 0) "/"
                         (or (:scorecard/compute-gated-refused sc) 0) "/"
                         (or (:scorecard/compute-quota-executed sc) 0) "\n"))
        (conj! lines (str "- liquidity-warifu R1/gated/loan: "
                         (or (:scorecard/liquidity-r1-dry sc) 0) "/"
                         (or (:scorecard/liquidity-gated-refused sc) 0) "/"
                         (or (:scorecard/liquidity-loan-executed sc) 0) "\n"))
        (conj! lines (str "- liquidity member-principal / cash-usd-micros: "
                         (or (:scorecard/liquidity-member-principal sc) 0) "/"
                         (or (:scorecard/liquidity-cash-usd-micros sc) 0) "\n"))
        (when-let [st (:scorecard/all-held-stress sc)]
          (conj! lines "\n### All-disclosure-held stress (priority #2)\n")
          (conj! lines (str "- held-subjects: " (:held-subjects st) "\n"))
          (conj! lines (str "- open-path gov flowable: " (:open-gov-flowable st) "\n"))
          (conj! lines (str "- all-held gov flowable: " (:gov-flowable st) "\n"))
          (conj! lines (str "- land-grant-executed: " (:land-grant-executed st) "\n"))
          (conj! lines (str "- live: " (:live st) " cash: " (:cash-usd-micros st) "\n"))))))
    (when-let [dep (:report/pages-deploy-status body)]
      (conj! lines "\n## Pages deploy (offline membrane, plan-only)\n")
      (conj! lines (str "- phase: " (:phase dep) "\n"))
      (conj! lines (str "- admissible: " (:admissible dep) "\n"))
      (conj! lines (str "- authorized-to-deploy: " (boolean (:authorized-to-deploy dep)) "\n"))
      (conj! lines (str "- package-ready: " (boolean (:package-ready dep true)) "\n"))
      (conj! lines (str "- wrangler-invoked: " (boolean (:wrangler-invoked dep)) "\n"))
      (conj! lines (str "- cloudflare-api-invoked: " (boolean (:cloudflare-api-invoked dep)) "\n"))
      (conj! lines (str "- operator-flag: " (or (:operator-flag dep) "FUCHI_ALLOW_PAGES_DEPLOY") "\n"))
      (conj! lines (str "- deployed: " (:deployed dep) "\n"))
      (conj! lines (str "- live: " (:live dep) " cash: " (:cash-usd-micros dep) "\n"))
      (conj! lines (str "- note: " (or (:note dep) "default refuse") "\n"))
      (when-let [rb (:operator-runbook dep)]
        (conj! lines "- operator runbook (OOB deploy only):\n")
        (doseq [s (or (:steps rb) [])]
          (conj! lines (str "  - " s "\n")))))
    (when-let [au (:report/pipeline-audit body)]
      (when (or (pos? (or (:runs au) 0)) (map? (:last-run au)))
        (conj! lines "\n## Pipeline audit summary (offline, append-only)\n")
        (conj! lines (str "- runs: " (or (:runs au) 0) "\n"))
        (conj! lines (str "- all-runs-live-refused: " (boolean (:all-runs-live-refused au)) "\n"))
        (conj! lines (str "- any-land-grant-executed: " (boolean (:any-land-grant-executed? au)) "\n"))
        (conj! lines (str "- last-run gov-flowable / gov-post-ratify: "
                         (or (:last-run-gov-flowable-committed-usd-micros au) 0) "/"
                         (or (:last-run-gov-post-ratify-committed-usd-micros au) 0) "\n"))
        (conj! lines (str "- last-run tenure-gov-flowable / tenure-gov-post-ratify: "
                         (or (:last-run-tenure-gov-flowable-committed-usd-micros au) 0) "/"
                         (or (:last-run-tenure-gov-post-ratify-committed-usd-micros au) 0) "\n"))
        (conj! lines (str "- last-run land-grant-executed: "
                         (or (:last-run-housing-land-grant-executed au) 0)
                         " (post-ratify plan keeps land-grant=false)\n"))
        (conj! lines (str "- last-run R2 refused/executed: "
                         (or (:last-run-r2-refused au) 0) "/"
                         (or (:last-run-r2-executed au) 0)
                         " all-r2-not-executed="
                         (boolean (:last-run-all-r2-not-executed au true)) "\n"))
        (conj! lines (str "- last-run SS rails-gated / all-rails-gated-refused: "
                         (or (:last-run-ss-rails-gated-count au) 0) "/"
                         (boolean (:last-run-ss-all-rails-gated-refused au true)) "\n"))
        (conj! lines (str "- last-run SS all-r2-not-executed / l0-published: "
                         (boolean (:last-run-ss-all-r2-not-executed au true)) "/"
                         (boolean (:last-run-ss-l0-published au)) "\n"))
        (conj! lines (str "- cumulative liquidity member-principal / cash-usd-micros: "
                         (or (:total-liquidity-member-principal au) 0) "/"
                         (or (:total-liquidity-cash-usd-micros au) 0) "\n"))
        (conj! lines (str "- live: " (boolean (:live au))
                         " cash: " (or (:cash-usd-micros au) 0) "\n"))))
    (when-let [sp (:report/ss-priority-path body)]
      (when (and (map? sp) (not (:error sp)))
        (conj! lines "\n## SS priority path (offline L0 + disclosure + mitsuho/hikari)\n")
        (conj! lines (str "- path: " (or (:path sp) "ss-offline-inkind-rails") "\n"))
        (conj! lines (str "- did: " (last-seg (:did sp)) "\n"))
        (conj! lines (str "- (1) L0 stage/published/token-stub: "
                         (:l0-stage sp) "/"
                         (boolean (:l0-published sp)) "/"
                         (or (:l0-token-stub sp) "—") "\n"))
        (conj! lines (str "- (1) ladder climb offline: "
                         (or (:ladder-from sp) "L0") "→"
                         (or (:ladder-to sp) "—")
                         " target=" (or (:ladder-target sp) "—")
                         " steps=" (or (:ladder-steps sp) 0)
                         " phase=" (or (:ladder-phase sp) "—")
                         " published=" (boolean (:ladder-published sp)) "\n"))
        (conj! lines (str "- (1) ladder rails-hint (care-first for 孫/子): "
                         (str/join "," (or (:ladder-rails-hint sp) []))
                         " first=" (or (:ladder-rails-hint-first sp) "—") "\n"))
        (conj! lines (str "- (2) disclosure open path: state="
                         (:disclosure-state sp)
                         " entitlements-may-flow="
                         (boolean (:entitlements-may-flow? sp)) "\n"))
        (conj! lines (str "- (2) held-stress: held="
                         (boolean (:held-stress-held? sp))
                         " food-r1-phase="
                         (or (:held-stress-food-phase sp) "—")
                         " ladder-refused="
                         (boolean (:held-stress-ladder-refused sp)) "\n"))
        (conj! lines (str "- (3) mitsuho R1/gated-admissible/produce-executed: "
                         (or (:mitsuho-r1-phase sp) "—") "/"
                         (boolean (:mitsuho-gated-admissible sp)) "/"
                         (boolean (:mitsuho-produce-executed sp)) "\n"))
        (conj! lines (str "- (3) hikari R1/gated-admissible/generate-executed: "
                         (or (:hikari-r1-phase sp) "—") "/"
                         (boolean (:hikari-gated-admissible sp)) "/"
                         (boolean (:hikari-generate-executed sp)) "\n"))
        (conj! lines (str "- (3) care/housing/tooling/compute/liquidity gated-admissible: "
                         (boolean (:care-gated-admissible sp)) "/"
                         (boolean (:housing-gated-admissible sp)) "/"
                         (boolean (:tooling-gated-admissible sp)) "/"
                         (boolean (:compute-gated-admissible sp)) "/"
                         (boolean (:liquidity-gated-admissible sp)) "\n"))
        (conj! lines (str "- (3) rails-gated-count/admissible/all-rails-gated-refused: "
                         (or (:rails-gated-count sp) 0) "/"
                         (or (:rails-gated-admissible-count sp) 0) "/"
                         (boolean (:all-rails-gated-refused sp)) "\n"))
        (conj! lines (str "- housing land-grant-executed / liquidity loan-executed/cash: "
                         (boolean (:housing-land-grant-executed sp)) "/"
                         (boolean (:liquidity-loan-executed sp)) "/"
                         (or (:liquidity-cash-usd-micros sp) 0) "\n"))
        (conj! lines (str "- R2 statuses/executed/all-not-executed: "
                         (or (:r2-status-count sp) 0) "/"
                         (or (:r2-executed-count sp) 0) "/"
                         (boolean (:all-r2-not-executed sp true)) "\n"))
        (conj! lines (str "- R2 food/energy executed: "
                         (boolean (:r2-food-executed sp)) "/"
                         (boolean (:r2-energy-executed sp))
                         " (phases "
                         (or (:r2-food-phase sp) "—") "/"
                         (or (:r2-energy-phase sp) "—") ")\n"))
        (conj! lines (str "- live: " (boolean (:live sp))
                         " cash: " (or (:cash-usd-micros sp) 0) "\n"))))
    (when-let [l0 (:report/l0-demo body)]
      (conj! lines "\n## L0 demo (offline)\n")
      (conj! lines (str "- did: " (last-seg (:did l0)) " stage=" (:stage l0)
                        " public=" (:public-person? l0) " cash=0 live=false\n")))
    (conj! lines "\n_No personal scores, ranks, or percentiles._\n")
    (apply str (persistent! lines))))

(defn report-html
  "Minimal static HTML public surface (facts only). No live, no scores."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0]
           :or {include-displacement-l0 true}}]
  (let [body (report-edn seed :include-l0-demo include-l0-demo :include-itonami include-itonami
                         :include-displacement-l0 include-displacement-l0)
        esc (fn [x] (-> (str x)
                        (str/replace "&" "&amp;")
                        (str/replace "<" "&lt;")
                        (str/replace ">" "&gt;")))
        rows (fn [header cells]
               (str "<tr>" (apply str (map #(str "<" header ">" (esc %) "</" header ">") cells)) "</tr>"))]
    (str
     "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/>"
     "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>"
     "<title>fuchi public surface (facts only)</title>"
     "<style>body{font-family:system-ui,sans-serif;margin:1.5rem;line-height:1.4}"
     "table{border-collapse:collapse;width:100%;margin:1rem 0}"
     "th,td{border:1px solid #ccc;padding:.4rem .6rem;text-align:left}"
     "th{background:#f4f4f4}.note{color:#444;font-size:.9rem}</style></head><body>"
     "<h1>fuchi — public surface (facts only)</h1>"
     "<p class=\"note\">Priority: wellbecoming &gt; mago(孫) &gt; ko(子) &gt; present. "
     "cash≡0. live=false. No personal scores or ranks. Floors are dry plans only.</p>"
     "<h2>Public persons</h2><table><thead>"
     (rows "th" ["did" "covenant" "public?" "disclosure" "rails" "imputed"])
     "</thead><tbody>"
     (apply str
            (for [f (:report/public-persons body)]
              (rows "td" [(last-seg (:did f)) (:covenant f) (:public-person? f)
                          (:disclosure-status f) (str/join "," (:rails f))
                          (:imputed-fact f)])))
     "</tbody></table>"
     "<h2>Dry floor plans (produce-executed=false)</h2><table><thead>"
     (rows "th" ["did" "kcal" "kWh" "care-h" "housing-mo" "tools" "GPU-h"])
     "</thead><tbody>"
     (apply str
            (for [r (:report/rail-packages body)]
              (rows "td" [(last-seg (:did r))
                          (or (get-in r [:food-floor :kcal-floor-yr]) "—")
                          (or (get-in r [:energy-floor :kwh-floor-yr]) "—")
                          (or (get-in r [:care-floor :care-hours-floor-yr]) "—")
                          (or (get-in r [:housing-floor :housing-months-floor-yr]) "—")
                          (or (get-in r [:tooling-floor :tool-units-floor-yr]) "—")
                          (or (get-in r [:compute-floor :gpu-hours-floor-yr]) "—")])))
     "</tbody></table>"
     "<h2>Displacement → earmark</h2><table><thead>"
     (rows "th" ["actor" "cohort" "displaced" "funded" "admissible" "earmark"])
     "</thead><tbody>"
     (apply str
            (for [d (:report/displacement body)]
              (rows "td" [(:displacing-actor d) (:cohort-id d) (:displaced-count d)
                          (:funded d) (:admissible d) (:earmark-usd-micros-yr d)])))
     "</tbody></table>"
     (when (seq (:report/itonami-displacement body))
       (str
        "<h2>itonami surplus bridge (offline)</h2><table><thead>"
        (rows "th" ["actor" "cohort" "displaced" "funded" "admissible"])
        "</thead><tbody>"
        (apply str
               (for [d (:report/itonami-displacement body)]
                 (rows "td" [(:displacing-actor d) (:cohort-id d) (:displaced-count d)
                             (:funded d) (:admissible d)])))
        "</tbody></table>"))
     (when (seq (get-in body [:report/displacement-l0 :packages]))
       (str
        "<h2>Displacement → L0→L4 enroll (offline)</h2>"
        "<p class=\"note\">Funded cohorts open L0 climb to L4 multi-gen (care/housing first); unfunded refuse. "
        "enrolled-subjects=" (get-in body [:report/displacement-l0 :enrolled-subjects])
        " refused-cohorts=" (get-in body [:report/displacement-l0 :refused-cohorts])
        " stages=" (pr-str (get-in body [:report/displacement-l0 :stage-counts]))
        " disclosure-open=" (or (get-in body [:report/displacement-l0 :disclosure-open]) 0)
        " disclosure-held=" (or (get-in body [:report/displacement-l0 :disclosure-held]) 0)
        " g2-admissible-cohorts=" (or (get-in body [:report/displacement-l0 :g2-admissible-cohorts]) 0)
        " earmark-total=" (or (get-in body [:report/displacement-l0 :earmark-usd-micros-yr]) 0)
        " committed-flowable-total=" (or (get-in body [:report/displacement-l0 :committed-usd-micros-yr]) 0)
        " committed-full-total=" (or (get-in body [:report/displacement-l0 :committed-full-usd-micros-yr]) 0)
        " tenure-subjects=" (or (get-in body [:report/displacement-l0 :tenure-subjects]) 0)
        " tenure-committed-flow=" (or (get-in body [:report/displacement-l0 :tenure-committed-usd-micros-yr]) 0)
        " gov-post-ratify-total=" (or (get-in body [:report/displacement-l0 :gov-post-ratify-usd-micros]) 0)
        " tenure-gov-post-ratify=" (or (get-in body [:report/displacement-l0 :tenure-gov-post-ratify-usd-micros]) 0)
        " land-grant-executed=" (or (get-in body [:report/displacement-l0 :housing-land-grant-executed]) 0)
        " (post-ratify keeps land-grant=false).</p>"
        "<table><thead>"
        (rows "th" ["actor" "cohort" "phase" "n" "g2" "funded" "earmark" "disc-o/h"
                    "L4-flow" "L4-full" "L4-post" "ten-n" "ten-flow" "ten-post"
                    "land-grant" "headroom"])
        "</thead><tbody>"
        (apply str
               (for [p (get-in body [:report/displacement-l0 :packages])]
                 (rows "td" [(:displacing-actor p) (:cohort-id p)
                             (:phase p) (:subject-count p)
                             (:g2-admissible p)
                             (boolean (:funded p))
                             (or (:earmark-usd-micros-yr p) 0)
                             (str (or (:disclosure-open p) 0) "/"
                                  (or (:disclosure-held p) 0))
                             (:committed-usd-micros-yr p)
                             (or (:committed-full-usd-micros-yr p) 0)
                             (or (:gov-post-ratify-usd-micros p) 0)
                             (or (:tenure-subjects p) 0)
                             (or (:tenure-committed-usd-micros-yr p) 0)
                             (or (:tenure-gov-post-ratify-usd-micros p) 0)
                             (or (:housing-land-grant-executed p) 0)
                             (:headroom-usd-micros-yr p)])))
        "</tbody></table>"))
     (when (get-in body [:report/displacement-scorecard :scorecard/id])
       (let [sc (:report/displacement-scorecard body)
             st (:scorecard/all-held-stress sc)]
         (str
          "<h2>SS scorecard (offline)</h2>"
          "<p class=\"note\">all-live-refused="
          (:scorecard/all-live-refused sc)
          " booked-entries=" (:scorecard/booked-entries sc)
          " committed-flowable=" (:scorecard/committed-usd-micros-yr sc)
          " gov-routes=" (pr-str (:scorecard/gov-route-counts sc))
          " gov-post-ratify=" (or (:scorecard/gov-post-ratify-committed-usd-micros sc) 0)
          " tenure-gov-post-ratify=" (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc) 0)
          " land-grant-executed=" (or (:scorecard/housing-land-grant-executed sc) 0)
          " r2-refused/executed=" (or (:scorecard/r2-refused sc) 0) "/"
          (or (:scorecard/r2-executed sc) 0)
          " all-r2-not-executed=" (boolean (:scorecard/all-r2-not-executed sc true))
          ". Housing held for Council; multi-gen substrate may dry-flow; post-ratify plan keeps land-grant=false; R2 execute default refuse.</p>"
          "<table><thead>"
          (rows "th" ["rail" "R1-dry" "gated-refused" "executed"])
          "</thead><tbody>"
          (rows "td" ["mitsuho food"
                      (or (:scorecard/mitsuho-r1-dry sc) 0)
                      (or (:scorecard/mitsuho-gated-refused sc) 0)
                      (or (:scorecard/mitsuho-produce-executed sc) 0)])
          (rows "td" ["hikari energy"
                      (or (:scorecard/hikari-r1-dry sc) 0)
                      (or (:scorecard/hikari-gated-refused sc) 0)
                      (or (:scorecard/hikari-generate-executed sc) 0)])
          (rows "td" ["care-iyashi"
                      (or (:scorecard/care-r1-dry sc) 0)
                      (or (:scorecard/care-gated-refused sc) 0)
                      (or (:scorecard/care-delivery-executed sc) 0)])
          (rows "td" ["housing-commons"
                      (or (:scorecard/housing-r1-dry sc) 0)
                      (or (:scorecard/housing-gated-refused sc) 0)
                      (or (:scorecard/housing-land-grant-executed sc) 0)])
          (rows "td" ["tooling-okaimono"
                      (or (:scorecard/tooling-r1-dry sc) 0)
                      (or (:scorecard/tooling-gated-refused sc) 0)
                      (or (:scorecard/tooling-fulfillment-executed sc) 0)])
          (rows "td" ["compute-murakumo"
                      (or (:scorecard/compute-r1-dry sc) 0)
                      (or (:scorecard/compute-gated-refused sc) 0)
                      (or (:scorecard/compute-quota-executed sc) 0)])
          (rows "td" ["liquidity-warifu"
                      (or (:scorecard/liquidity-r1-dry sc) 0)
                      (or (:scorecard/liquidity-gated-refused sc) 0)
                      (or (:scorecard/liquidity-loan-executed sc) 0)])
          "</tbody></table>"
          "<p class=\"note\">liquidity residual (housing Council-held): member-principal="
          (or (:scorecard/liquidity-member-principal sc) 0)
          " cash-usd-micros="
          (or (:scorecard/liquidity-cash-usd-micros sc) 0)
          " (fuchi never cash creditor). housing-council-held="
          (or (:scorecard/housing-council-held sc) 0)
          " land-grant-executed="
          (or (:scorecard/housing-land-grant-executed sc) 0)
          ".</p>"
          (when st
            (str
             "<h3>All-disclosure-held stress (priority #2)</h3>"
             "<p class=\"note\">held-subjects=" (:held-subjects st)
             " open-gov-flowable=" (:open-gov-flowable st)
             " all-held-gov-flowable=" (:gov-flowable st)
             " land-grant-executed=" (:land-grant-executed st)
             " live=" (:live st)
             " cash=" (:cash-usd-micros st) ".</p>")))))
     (when-let [dep (:report/pages-deploy-status body)]
       (str
        "<h2>Pages deploy (offline membrane, plan-only)</h2>"
        "<p class=\"note\">phase=" (:phase dep)
        " admissible=" (:admissible dep)
        " authorized-to-deploy=" (boolean (:authorized-to-deploy dep))
        " package-ready=" (boolean (:package-ready dep true))
        " wrangler-invoked=" (boolean (:wrangler-invoked dep))
        " cloudflare-api-invoked=" (boolean (:cloudflare-api-invoked dep))
        " operator-flag=" (or (:operator-flag dep) "FUCHI_ALLOW_PAGES_DEPLOY")
        " deployed=" (:deployed dep)
        " live=" (:live dep)
        " cash=" (:cash-usd-micros dep)
        ". " (or (:note dep) "default refuse — static package only")
        " Gated plan still requires flag+operator-did and never invokes wrangler here;"
        " actual deploy is operator out-of-band."
        "</p>"))
     (when-let [au (:report/pipeline-audit body)]
       (when (or (pos? (or (:runs au) 0)) (map? (:last-run au)))
         (str
          "<h2>Pipeline audit summary (offline)</h2>"
          "<p class=\"note\">runs=" (or (:runs au) 0)
          " all-runs-live-refused=" (boolean (:all-runs-live-refused au))
          " any-land-grant-executed=" (boolean (:any-land-grant-executed? au))
          " last-run gov-flowable/gov-post-ratify="
          (or (:last-run-gov-flowable-committed-usd-micros au) 0) "/"
          (or (:last-run-gov-post-ratify-committed-usd-micros au) 0)
          " tenure-gov-flowable/tenure-gov-post-ratify="
          (or (:last-run-tenure-gov-flowable-committed-usd-micros au) 0) "/"
          (or (:last-run-tenure-gov-post-ratify-committed-usd-micros au) 0)
          " land-grant-executed="
          (or (:last-run-housing-land-grant-executed au) 0)
          " (post-ratify keeps land-grant=false)."
          " last-run R2 refused/executed="
          (or (:last-run-r2-refused au) 0) "/"
          (or (:last-run-r2-executed au) 0)
          " all-r2-not-executed="
          (boolean (:last-run-all-r2-not-executed au true))
          " last-run SS rails-gated/all-refused="
          (or (:last-run-ss-rails-gated-count au) 0) "/"
          (boolean (:last-run-ss-all-rails-gated-refused au true))
          " ss-all-r2-not-executed="
          (boolean (:last-run-ss-all-r2-not-executed au true))
          " liquidity member-principal/cash="
          (or (:total-liquidity-member-principal au) 0) "/"
          (or (:total-liquidity-cash-usd-micros au) 0)
          " live=" (boolean (:live au))
          " cash=" (or (:cash-usd-micros au) 0)
          ".</p>")))
     (when-let [sp (:report/ss-priority-path body)]
       (when (and (map? sp) (not (:error sp)))
         (str
          "<h2>SS priority path (offline)</h2>"
          "<p class=\"note\">(1) L0 stage=" (:l0-stage sp)
          " published=" (boolean (:l0-published sp))
          " token-stub=" (or (:l0-token-stub sp) "—")
          " ladder=" (or (:ladder-from sp) "L0") "→" (or (:ladder-to sp) "—")
          " rails-hint-first=" (or (:ladder-rails-hint-first sp) "—")
          " ladder-published=" (boolean (:ladder-published sp))
          ". (2) disclosure=" (:disclosure-state sp)
          " entitlements-may-flow=" (boolean (:entitlements-may-flow? sp))
          " held-stress-held=" (boolean (:held-stress-held? sp))
          " held-food-r1=" (or (:held-stress-food-phase sp) "—")
          " held-ladder-refused=" (boolean (:held-stress-ladder-refused sp))
          ". (3) mitsuho/hikari gated="
          (boolean (:mitsuho-gated-admissible sp)) "/"
          (boolean (:hikari-gated-admissible sp))
          " care/housing/tooling/compute/liquidity gated="
          (boolean (:care-gated-admissible sp)) "/"
          (boolean (:housing-gated-admissible sp)) "/"
          (boolean (:tooling-gated-admissible sp)) "/"
          (boolean (:compute-gated-admissible sp)) "/"
          (boolean (:liquidity-gated-admissible sp))
          " rails-gated=" (or (:rails-gated-count sp) 0)
          " all-rails-gated-refused=" (boolean (:all-rails-gated-refused sp))
          " land-grant=" (boolean (:housing-land-grant-executed sp))
          " R2 statuses/executed="
          (or (:r2-status-count sp) 0) "/"
          (or (:r2-executed-count sp) 0)
          " all-r2-not-executed=" (boolean (:all-r2-not-executed sp true))
          " live=" (boolean (:live sp))
          " cash=" (or (:cash-usd-micros sp) 0)
          ".</p>")))
     "<p class=\"note\">G2: no live displacement without a funded cohort. "
     "Recipient scores are unrepresentable. Live rails default refuse. cash≡0.</p>"
     "</body></html>")))

#?(:clj
   (defn write-report!
     "Write out/public-surface.{md,edn,html} from seed path."
     ([]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
            seed (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
            outd (io/file actor "out")]
        (.mkdirs outd)
        (spit (io/file outd "public-surface.md")
              (report-md seed :include-l0-demo true :include-itonami true))
        (spit (io/file outd "public-surface.edn")
              (pr-str (report-edn seed :include-l0-demo true :include-itonami true)))
        (spit (io/file outd "public-surface.html")
              (report-html seed :include-l0-demo true :include-itonami true))
        {:md (str (io/file outd "public-surface.md"))
         :edn (str (io/file outd "public-surface.edn"))
         :html (str (io/file outd "public-surface.html"))}))))
