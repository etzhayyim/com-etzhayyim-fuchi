(ns fuchi.methods.compute-murakumo-receive
  "compute_murakumo_receive.cljc — offline dry-run of murakumo accepting compute intent.

  Does NOT allocate mesh quota, does NOT go live, does NOT move cash. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.live-gate :as live-gate]))

(def PROVIDER-DID "murakumo")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["compute-receive-ack-supports-learning"
   "not-a-happiness-score"
   "mesh-quota-not-invoked"])

(defn- assert-intent! [intent]
  (when-not (= "compute-murakumo" (:rail-kind intent))
    (throw (ex-info "murakumo only receives compute-murakumo rails" {:rail (:rail-kind intent)})))
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
             :rail-kind "compute-murakumo"
             :alloc-id (:alloc-id intent)
             :subject-did subject-did
             :imputed-usd-micros-yr (:imputed-usd-micros-yr intent)
             :received-at-offline true
             :quota-invoked false
             :published false
             :live false
             :cash-usd-micros 0
             :server-held-key false
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :score-surface []
             :note "murakumo dry-ack only — mesh quota R2 not invoked"}]
    (pp/assert-no-public-scores! pkg)
    pkg))

(defn receive-from-r1-package [r1-pkg]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot receive refused package" r1-pkg)))
  (dry-receive (:intent r1-pkg) :subject-did (:subject-did r1-pkg)))

(defn gated-receive-plan
  [r1-pkg gate & {:keys [env]}]
  (let [plan (compute/gated-live-plan r1-pkg gate :env env)
        ack (dry-receive (:intent plan) :subject-did (:subject-did plan))]
    (assoc ack
           :phase :gated-ack-plan
           :authorized-to-publish (boolean (:authorized-to-publish plan))
           :live false
           :quota-invoked false
           :published false
           :note "gated capability presented; murakumo quota still not invoked")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
