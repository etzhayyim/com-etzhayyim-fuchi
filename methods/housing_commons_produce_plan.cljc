(ns fuchi.methods.housing-commons-produce-plan
  "housing_commons_produce_plan.cljc — dry HOUSING PLAN for 子・孫 wellbecoming (R2 design).

  After dry-receive, plan multi-gen housing-month floor from imputed USD micros.
  Does NOT grant land, live=false, cash≡0, no scores. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.housing-commons-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only: ~$1000/mo illustrative shelter proxy → 0.001 month per USD.
(def MONTHS-PER-USD-METHOD "v1-shelter-proxy-illustrative")
(def MONTHS-PER-USD 0.001)

(def MULTI-GEN-FACTS
  ["housing-months-floor-supports-ko-and-mago"
   "illustrative-conversion-not-a-score"
   "land-grant-not-executed"
   "commons-not-private-equity"])

(defn- micros->housing-months-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd MONTHS-PER-USD 12.0)))))

(defn dry-produce-plan
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:land-grant-invoked receive-ack)
    (throw (ex-info "receive already claimed land grant — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        months (micros->housing-months-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "housing-commons"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :housing-months-floor-yr months
              :months-method MONTHS-PER-USD-METHOD
              :produce-executed false
              :land-grant-invoked false
              :land-grant-executed false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry housing plan only — no LANDS.md grant executed"}]
    (pp/assert-no-public-scores! plan)
    plan))

(defn plan-from-r1 [r1-pkg]
  (let [ack (recv/receive-from-r1-package r1-pkg)]
    (dry-produce-plan ack)))

(defn gated-produce-plan
  [r1-pkg gate & {:keys [env]}]
  (let [ack (recv/gated-receive-plan r1-pkg gate :env env)
        plan (dry-produce-plan ack)]
    (assoc plan
           :phase :gated-produce-plan
           :authorized-to-publish (boolean (:authorized-to-publish ack))
           :produce-executed false
           :land-grant-executed false
           :live false
           :published false
           :note "gated plan authorized; commons-land grant still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
