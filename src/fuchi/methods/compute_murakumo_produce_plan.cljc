(ns fuchi.methods.compute-murakumo-produce-plan
  "compute_murakumo_produce_plan.cljc — dry COMPUTE PLAN for learning/vocation (R2 design).

  After dry-receive, plan GPU-hour floor from imputed USD micros.
  Does NOT allocate mesh quota, live=false, cash≡0, no scores. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.compute-murakumo-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only: ~$2/GPU-hr proxy → 0.5 hr per USD.
(def GPU-HR-PER-USD-METHOD "v1-mesh-gpu-proxy-illustrative")
(def GPU-HR-PER-USD 0.5)

(def MULTI-GEN-FACTS
  ["gpu-hours-floor-supports-learning-and-vocation"
   "illustrative-conversion-not-a-score"
   "mesh-quota-not-executed"
   "wellbecoming-substrate"])

(defn- micros->gpu-hours-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd GPU-HR-PER-USD)))))

(defn dry-produce-plan
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:quota-invoked receive-ack)
    (throw (ex-info "receive already claimed quota — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        hrs (micros->gpu-hours-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "compute-murakumo"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :gpu-hours-floor-yr hrs
              :gpu-hours-method GPU-HR-PER-USD-METHOD
              :produce-executed false
              :quota-invoked false
              :quota-executed false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry compute plan only — no murakumo quota allocated"}]
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
           :quota-executed false
           :live false
           :published false
           :note "gated plan authorized; murakumo quota still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
