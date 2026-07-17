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
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

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

(defn displacement-l0-public-summary
  "Facts-only projection of displacement→L0/L2 batch (no subject scores)."
  [batch]
  (let [subjects (mapcat :subjects (or (:packages batch) []))
        pkgs (mapv
              (fn [p]
                (let [stages (frequencies (map :stage (:subjects p)))
                      row {:cohort-id (:cohort-id p)
                           :displacing-actor (:displacing-actor p)
                           :phase (name (:phase p))
                           :subject-count (count (:subjects p))
                           :stages stages
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
     :packages pkgs
     :cash-usd-micros 0
     :live false
     :score-surface []
     :priority-stack PRIORITY-STACK}))

(defn report-edn
  "Full facts-only report structure."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0]
           :or {include-displacement-l0 true}}]
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
        dl0-sum (when include-displacement-l0
                  #?(:clj
                     (try
                       (displacement-l0-public-summary
                        (dl0/run-default-seed :max-slots 2))
                       (catch Exception _ nil))
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
              :report/l0-demo (when include-l0-demo
                                (l0-demo-fact "did:web:etzhayyim.com:member:lot"))}]
    (doseq [f facts] (pp/assert-no-public-scores! f))
    (doseq [d drows] (pp/assert-no-public-scores! d))
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
        (conj! lines "\n## Displacement → L0→L3 enroll (offline; G2 gated)\n")
        (conj! lines (str "admissible-cohorts=" (:admissible-cohorts dl0)
                          " refused-cohorts=" (:refused-cohorts dl0)
                          " enrolled-subjects=" (:enrolled-subjects dl0)
                          " stages=" (pr-str (:stage-counts dl0)) "\n"))
        (conj! lines "| actor | cohort | phase | subjects | stages |\n|---|---|---|---|---|\n")
        (doseq [p (:packages dl0)]
          (conj! lines
                 (str "| " (:displacing-actor p) " | " (:cohort-id p) " | "
                      (:phase p) " | " (:subject-count p) " | "
                      (pr-str (:stages p)) " |\n")))))
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
        "<h2>Displacement → L0→L3 enroll (offline)</h2>"
        "<p class=\"note\">Funded cohorts open L0 climb to L3 multi-gen+vocation floors; unfunded refuse. "
        "enrolled-subjects=" (get-in body [:report/displacement-l0 :enrolled-subjects])
        " refused-cohorts=" (get-in body [:report/displacement-l0 :refused-cohorts])
        " stages=" (pr-str (get-in body [:report/displacement-l0 :stage-counts])) ".</p>"
        "<table><thead>"
        (rows "th" ["actor" "cohort" "phase" "subjects" "stages"])
        "</thead><tbody>"
        (apply str
               (for [p (get-in body [:report/displacement-l0 :packages])]
                 (rows "td" [(:displacing-actor p) (:cohort-id p)
                             (:phase p) (:subject-count p) (pr-str (:stages p))])))
        "</tbody></table>"))
     "<p class=\"note\">G2: no live displacement without a funded cohort. "
     "Recipient scores are unrepresentable. Live rails default refuse.</p>"
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
