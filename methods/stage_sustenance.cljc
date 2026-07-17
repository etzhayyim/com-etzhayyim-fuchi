(ns fuchi.methods.stage-sustenance
  "stage_sustenance.cljc — offline stage-aware in-kind floor packages.

  Maps Liberation Ladder stage rails-hint → R1 dry packages + produce plans.
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

(defn- package+plan
  [kind subject-did imputed person hold]
  (let [opts {:alloc-id (str kind "-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr (long imputed)
              :person person
              :hold-machine hold}]
    (case kind
      "food" (let [pkg (food/r1-dry-package opts)]
               {:package pkg
                :plan (when (not= :refused (:phase pkg)) (mprod/plan-from-r1 pkg))
                :execute-leg "produce"})
      "energy" (let [pkg (energy/r1-dry-package opts)]
                 {:package pkg
                  :plan (when (not= :refused (:phase pkg)) (hprod/plan-from-r1 pkg))
                  :execute-leg "generate"})
      "care" (let [pkg (care/r1-dry-package opts)]
               {:package pkg
                :plan (when (not= :refused (:phase pkg)) (cprod/plan-from-r1 pkg))
                :execute-leg "produce"})
      "housing" (let [pkg (housing/r1-dry-package opts)]
                  {:package pkg
                   :plan (when (not= :refused (:phase pkg)) (housprod/plan-from-r1 pkg))
                   :execute-leg "grant"})
      "tooling" (let [pkg (tooling/r1-dry-package opts)]
                  {:package pkg
                   :plan (when (not= :refused (:phase pkg)) (tprod/plan-from-r1 pkg))
                   :execute-leg "produce"})
      "compute" (let [pkg (compute/r1-dry-package opts)]
                  {:package pkg
                   :plan (when (not= :refused (:phase pkg)) (cmpprod/plan-from-r1 pkg))
                   :execute-leg "quota"})
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

(defn build-for-stage
  "Build offline rail packages for a person's stage. imputed-overrides: map kind→micros."
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
        out {:stage stage
             :subject-did did
             :rails kinds
             :packages rails
             :floor-usd-micros-yr total
             :label (get-in ladder/STAGE-FACTS [stage :label])
             :multi-gen-fact (get-in ladder/STAGE-FACTS [stage :multi-gen])
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "stage sustenance dry only — R2 execute refused"}]
    (pp/assert-no-public-scores! out)
    out))

(defn public-floor-row
  "Facts-only row for reports (no nested intents with scores)."
  [stage-pkg]
  (let [floors (into {}
                     (map (fn [[k v]]
                            [k (or (:floor v) {})])
                          (:packages stage-pkg)))
        row {:subject-did (:subject-did stage-pkg)
             :stage (:stage stage-pkg)
             :label (:label stage-pkg)
             :rails (:rails stage-pkg)
             :floors floors
             :floor-usd-micros-yr (:floor-usd-micros-yr stage-pkg)
             :cash-usd-micros 0
             :live false
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! row)
    row))
