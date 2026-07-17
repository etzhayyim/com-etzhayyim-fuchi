(ns fuchi.methods.mitsuho-produce-plan
  "mitsuho_produce_plan.cljc — dry PRODUCE PLAN for staple food (R2 design, not live).

  After dry-receive, plan multi-gen kcal floor from imputed USD micros.
  Does NOT ship food, does NOT call farms, live=false, cash≡0, no scores.
  Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.mitsuho-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only conversion for dry plan (not nutritional medical advice).
;; ~$1 → ~500 kcal floor proxy at staple prices (illustrative, method-versioned).
(def KCAL-PER-USD-MICROS-YR-METHOD "v1-staple-proxy-illustrative")
(def KCAL-PER-USD 500.0)

(def MULTI-GEN-FACTS
  ["kcal-floor-plan-supports-caregiver-and-child-households"
   "illustrative-conversion-not-a-score"
   "produce-not-executed"
   "wellbecoming-substrate-not-wellbeing-rank"])

(defn- micros->kcal-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd KCAL-PER-USD 365.0)))))

(defn dry-produce-plan
  "Build produce plan from a dry-receive ack. produce-executed stays false."
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:produce-invoked receive-ack)
    (throw (ex-info "receive already claimed produce — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        kcal (micros->kcal-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "food-mitsuho"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :kcal-floor-yr kcal
              :kcal-method KCAL-PER-USD-MICROS-YR-METHOD
              :produce-executed false
              :produce-invoked false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry produce plan only — no farm dispatch, no shipment"}]
    (pp/assert-no-public-scores! plan)
    plan))

(defn plan-from-r1
  "R1 package → dry-receive → dry-produce-plan (full offline food path fragment)."
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
           :live false
           :published false
           :note "gated plan authorized; produce still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
