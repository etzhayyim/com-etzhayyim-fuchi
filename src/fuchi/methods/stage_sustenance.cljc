(ns fuchi.methods.stage-sustenance
  "stage_sustenance.cljc — offline stage-aware in-kind floor packages.

  Maps Liberation Ladder stage rails-hint → R1 dry packages + produce plans
  + R1→gated-live DESIGN status (default refuse) + R2 execute refuse.
  Multi-gen priority: care/housing/food/energy before vocation rails.
  live=false, cash≡0, produce-executed=false. Portable .cljc."
  (:require [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-mitsuho :as food]
            [fuchi.methods.rail-hikari :as energy]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.housing-commons-produce-plan :as housprod]
            [fuchi.methods.tooling-okaimono-produce-plan :as tprod]
            [fuchi.methods.compute-murakumo-produce-plan :as cmpprod]
            [fuchi.methods.r2-execute :as r2]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Default imputed floors (USD micros / yr) — accounting facts, not scores.
(def DEFAULT-IMPUTED
  {"food" 2000000000
   "energy" 800000000
   "care" 1000000000
   "housing" 6000000000
   "tooling" 500000000
   "compute" 400000000})

(defn rails-for-stage [stage]
  (or (get-in ladder/STAGE-FACTS [(ladder/normalize-stage stage) :rails-hint])
      ["care"]))

(defn- gated-for
  "R1→gated-live DESIGN status for a stage rail package (default refuse)."
  [kind pkg hold]
  (when (and pkg (not= :refused (:phase pkg)))
    (case kind
      "food" (food/gated-live-status pkg :hold-machine hold)
      "energy" (energy/gated-live-status pkg :hold-machine hold)
      "care" (care/gated-live-status pkg :hold-machine hold)
      "housing" (housing/gated-live-status pkg :hold-machine hold)
      "tooling" (tooling/gated-live-status pkg :hold-machine hold)
      "compute" (compute/gated-live-status pkg :hold-machine hold)
      nil)))

(defn- package+plan
  [kind subject-did imputed person hold]
  (let [opts {:alloc-id (str kind "-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr (long imputed)
              :person person
              :hold-machine hold}]
    (case kind
      "food" (let [pkg (food/r1-dry-package opts)
                   plan (when (not= :refused (:phase pkg)) (mprod/plan-from-r1 pkg))]
               {:package pkg
                :plan plan
                :execute-leg "produce"
                :gated (gated-for "food" pkg hold)})
      "energy" (let [pkg (energy/r1-dry-package opts)
                     plan (when (not= :refused (:phase pkg)) (hprod/plan-from-r1 pkg))]
                 {:package pkg
                  :plan plan
                  :execute-leg "generate"
                  :gated (gated-for "energy" pkg hold)})
      "care" (let [pkg (care/r1-dry-package opts)
                   plan (when (not= :refused (:phase pkg)) (cprod/plan-from-r1 pkg))]
               {:package pkg
                :plan plan
                :execute-leg "produce"
                :gated (gated-for "care" pkg hold)})
      "housing" (let [pkg (housing/r1-dry-package opts)
                      plan (when (not= :refused (:phase pkg)) (housprod/plan-from-r1 pkg))]
                  {:package pkg
                   :plan plan
                   :execute-leg "grant"
                   :gated (gated-for "housing" pkg hold)})
      "tooling" (let [pkg (tooling/r1-dry-package opts)
                      plan (when (not= :refused (:phase pkg)) (tprod/plan-from-r1 pkg))]
                  {:package pkg
                   :plan plan
                   :execute-leg "produce"
                   :gated (gated-for "tooling" pkg hold)})
      "compute" (let [pkg (compute/r1-dry-package opts)
                      plan (when (not= :refused (:phase pkg)) (cmpprod/plan-from-r1 pkg))]
                  {:package pkg
                   :plan plan
                   :execute-leg "quota"
                   :gated (gated-for "compute" pkg hold)})
      nil)))

(defn floor-facts [plan]
  (when plan
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

(defn gated-summary
  "Aggregate gated-live DESIGN facts from a stage package (facts only)."
  [stage-pkg]
  (let [gated (keep (fn [[k v]]
                      (when-let [g (:gated v)]
                        {:rail k
                         :admissible (boolean (:admissible g))
                         :phase (when-let [p (:phase g)] (name p))
                         :land-grant-executed (boolean (:land-grant-executed g))
                         :produce-executed (boolean (:produce-executed g false))
                         :live (boolean (:live g))
                         :cash-usd-micros 0}))
                    (or (:packages stage-pkg) {}))
        n (count gated)
        adm (count (filter :admissible gated))
        out {:gated-count n
             :gated-admissible-count adm
             :all-gated-refused (and (pos? n) (zero? adm))
             :care-gated-admissible
             (boolean (some #(and (= "care" (:rail %)) (:admissible %)) gated))
             :food-gated-admissible
             (boolean (some #(and (= "food" (:rail %)) (:admissible %)) gated))
             :energy-gated-admissible
             (boolean (some #(and (= "energy" (:rail %)) (:admissible %)) gated))
             :housing-gated-admissible
             (boolean (some #(and (= "housing" (:rail %)) (:admissible %)) gated))
             :housing-land-grant-executed
             (boolean (some #(and (= "housing" (:rail %)) (:land-grant-executed %)) gated))
             :by-rail (into {} (map (fn [g] [(:rail g) (dissoc g :rail)]) gated))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "stage R1→gated-live DESIGN — default refuse; no side-effects"}]
    (pp/assert-no-public-scores! (dissoc out :by-rail))
    out))

(defn build-for-stage
  "Build offline rail packages for a person's stage.
   Includes R1 dry + produce plan + gated-live DESIGN status + R2 refuse.
   imputed-overrides: map kind→micros."
  [person hold-machine & {:keys [imputed-overrides]}]
  (let [stage (ladder/normalize-stage (or (:stage person) "L0"))
        kinds (rails-for-stage stage)
        did (:did person)
        overrides (or imputed-overrides {})
        rails
        (into {}
              (keep
               (fn [kind]
                 (let [imp (long (or (get overrides kind)
                                     (get DEFAULT-IMPUTED kind)
                                     0))
                       built (when (pos? imp)
                               (package+plan kind did imp person hold-machine))]
                   (when built
                     [kind (assoc built
                                  :imputed-usd-micros-yr imp
                                  :floor (floor-facts (:plan built))
                                  :r2 (when (:plan built)
                                        (r2/refuse-without-gate
                                         (:execute-leg built)
                                         (:plan built))))])))
               kinds))
        total (reduce + 0 (map :imputed-usd-micros-yr (vals rails)))
        r2-phases (keep (fn [[_ v]] (get-in v [:r2 :phase])) rails)
        gsum (gated-summary {:packages rails})
        out {:stage stage
             :subject-did did
             :rails kinds
             :packages rails
             :floor-usd-micros-yr total
             :label (get-in ladder/STAGE-FACTS [stage :label])
             :multi-gen-fact (get-in ladder/STAGE-FACTS [stage :multi-gen])
             :gated-count (:gated-count gsum)
             :gated-admissible-count (:gated-admissible-count gsum)
             :all-gated-refused (:all-gated-refused gsum)
             :gated-summary gsum
             :r2-count (count r2-phases)
             :r2-all-refused (and (seq r2-phases) (every? #(= :refused %) r2-phases))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "stage sustenance dry — gated-live DESIGN refuse + R2 refuse"}]
    (pp/assert-no-public-scores! (dissoc out :packages :gated-summary :rails))
    (when-let [gs (:gated-summary out)]
      (pp/assert-no-public-scores! (dissoc gs :by-rail)))
    out))

(defn public-floor-row
  "Facts-only row for reports (no nested intents with scores)."
  [stage-pkg]
  (let [floors (into {}
                     (map (fn [[k v]]
                            [k (or (:floor v) {})])
                          (:packages stage-pkg)))
        gsum (or (:gated-summary stage-pkg) (gated-summary stage-pkg))
        row {:subject-did (:subject-did stage-pkg)
             :stage (:stage stage-pkg)
             :label (:label stage-pkg)
             :rails (:rails stage-pkg)
             :floors floors
             :floor-usd-micros-yr (:floor-usd-micros-yr stage-pkg)
             :gated-count (or (:gated-count stage-pkg) (:gated-count gsum) 0)
             :gated-admissible-count
             (or (:gated-admissible-count stage-pkg)
                 (:gated-admissible-count gsum) 0)
             :all-gated-refused
             (boolean (or (:all-gated-refused stage-pkg)
                          (:all-gated-refused gsum)))
             :r2-all-refused (boolean (:r2-all-refused stage-pkg true))
             :cash-usd-micros 0
             :live false
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! row)
    row))
