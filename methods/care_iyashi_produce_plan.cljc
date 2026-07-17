(ns fuchi.methods.care-iyashi-produce-plan
  "care_iyashi_produce_plan.cljc — dry CARE PLAN for 子・孫 wellbecoming (R2 design, not live).

  After dry-receive, plan multi-gen care-hours floor from imputed USD micros.
  Does NOT deliver care, does NOT call iyashi clinical live, live=false, cash≡0, no scores.
  Priority stack: wellbecoming > mago > ko > present. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.care-iyashi-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only conversion (illustrative). ~$25/hr household care proxy → 0.04 h per USD.
(def HOURS-PER-USD-METHOD "v1-household-care-proxy-illustrative")
(def HOURS-PER-USD 0.04)

(def MULTI-GEN-FACTS
  ["care-hours-floor-supports-ko-and-mago"
   "illustrative-conversion-not-a-score"
   "care-delivery-not-executed"
   "wellbecoming-substrate-not-wellbeing-rank"])

(defn- micros->care-hours-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd HOURS-PER-USD 365.0)))))

(defn dry-produce-plan
  "Build care plan from dry-receive ack. care-delivery-executed stays false."
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:care-delivery-invoked receive-ack)
    (throw (ex-info "receive already claimed care delivery — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        hrs (micros->care-hours-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "care-iyashi"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :care-hours-floor-yr hrs
              :hours-method HOURS-PER-USD-METHOD
              :produce-executed false
              :care-delivery-invoked false
              :care-delivery-executed false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry care plan only — no clinical dispatch, no hours fulfilled"}]
    (pp/assert-no-public-scores! plan)
    plan))

(defn plan-from-r1
  [r1-pkg]
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
           :care-delivery-executed false
           :live false
           :published false
           :note "gated plan authorized; iyashi care delivery still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
