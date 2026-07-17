(ns fuchi.methods.hikari-produce-plan
  "hikari_produce_plan.cljc — dry GENERATE PLAN for renewable energy (R2 design, not live).

  After dry-receive, plan multi-gen kWh floor from imputed USD micros.
  Does NOT generate power, does NOT call hikari live, live=false, cash≡0, no scores.
  Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.hikari-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only conversion for dry plan (not engineering guarantee).
;; ~$0.20/kWh household proxy → 5 kWh per USD (illustrative, method-versioned).
(def KWH-PER-USD-METHOD "v1-household-proxy-illustrative")
(def KWH-PER-USD 5.0)

(def MULTI-GEN-FACTS
  ["kwh-floor-plan-supports-care-and-learning"
   "no-fossil-no-nuclear"
   "illustrative-conversion-not-a-score"
   "generate-not-executed"
   "wellbecoming-substrate-not-wellbeing-rank"])

(defn- micros->kwh-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd KWH-PER-USD 365.0)))))

(defn dry-produce-plan
  "Build generate plan from a dry-receive ack. generate-executed stays false."
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:generate-invoked receive-ack)
    (throw (ex-info "receive already claimed generate — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        kwh (micros->kwh-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "energy-hikari"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :kwh-floor-yr kwh
              :kwh-method KWH-PER-USD-METHOD
              :produce-executed false
              :generate-invoked false
              :generate-executed false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry generate plan only — no plant dispatch, no grid injection"}]
    (pp/assert-no-public-scores! plan)
    plan))

(defn plan-from-r1
  "R1 package → dry-receive → dry-produce-plan (full offline energy path fragment)."
  [r1-pkg]
  (let [ack (recv/receive-from-r1-package r1-pkg)]
    (dry-produce-plan ack)))

(defn gated-produce-plan
  "Gated capability → still dry produce plan (no execution)."
  [r1-pkg gate & {:keys [env]}]
  (let [ack (recv/gated-receive-plan r1-pkg gate :env env)
        plan (dry-produce-plan ack)]
    (assoc plan
           :phase :gated-produce-plan
           :authorized-to-publish (boolean (:authorized-to-publish ack))
           :produce-executed false
           :generate-executed false
           :live false
           :published false
           :note "gated plan authorized; hikari generate still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
