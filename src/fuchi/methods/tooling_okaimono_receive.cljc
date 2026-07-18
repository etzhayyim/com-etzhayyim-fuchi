(ns fuchi.methods.tooling-okaimono-receive
  "tooling_okaimono_receive.cljc — offline dry-run of okaimono 御買物 accepting tooling intent.

  Does NOT fulfill tools, does NOT go live, does NOT move cash. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.live-gate :as live-gate]))

(def PROVIDER-DID "did:web:etzhayyim.com:actor:okaimono")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["tool-receive-ack-supports-vocation-recovery"
   "not-a-happiness-score"
   "fulfillment-not-invoked"])

(defn- assert-intent! [intent]
  (when-not (= "tooling-okaimono" (:rail-kind intent))
    (throw (ex-info "okaimono only receives tooling-okaimono rails" {:rail (:rail-kind intent)})))
  (when-not (= PROVIDER-DID (:provider-did intent))
    (throw (ex-info "provider DID mismatch" {:provider (:provider-did intent)})))
  (when-not (= 0 (:cash-usd-micros intent))
    (throw (ex-info "cash≡0" {})))
  (when (:server-held-key intent)
    (throw (ex-info "no-server-key" {})))
  (when (:published intent)
    (throw (ex-info "G10: cannot receive already-published intent in dry-run" {})))
  true)

(defn dry-receive
  [intent & {:keys [subject-did]}]
  (assert-intent! intent)
  (let [pkg {:phase :dry-ack
             :provider-did PROVIDER-DID
             :rail-kind "tooling-okaimono"
             :alloc-id (:alloc-id intent)
             :subject-did subject-did
             :imputed-usd-micros-yr (:imputed-usd-micros-yr intent)
             :received-at-offline true
             :fulfillment-invoked false
             :published false
             :live false
             :cash-usd-micros 0
             :server-held-key false
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :score-surface []
             :note "okaimono dry-ack only — fulfillment R2 not invoked"}]
    (pp/assert-no-public-scores! pkg)
    pkg))

(defn receive-from-r1-package [r1-pkg]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot receive refused package" r1-pkg)))
  (dry-receive (:intent r1-pkg) :subject-did (:subject-did r1-pkg)))

(defn gated-receive-plan
  [r1-pkg gate & {:keys [env]}]
  (let [plan (tooling/gated-live-plan r1-pkg gate :env env)
        ack (dry-receive (:intent plan) :subject-did (:subject-did plan))]
    (assoc ack
           :phase :gated-ack-plan
           :authorized-to-publish (boolean (:authorized-to-publish plan))
           :live false
           :fulfillment-invoked false
           :published false
           :note "gated capability presented; okaimono fulfillment still not invoked")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
