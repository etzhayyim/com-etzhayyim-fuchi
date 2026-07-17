(ns fuchi.methods.care-iyashi-receive
  "care_iyashi_receive.cljc — offline dry-run of iyashi 癒 accepting a care provisioning intent.

  Sibling of mitsuho/hikari receive. Does NOT deliver care, does NOT go live, does NOT
  move cash. Acknowledges multi-gen care floor facts (子・孫). Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.live-gate :as live-gate]))

(def PROVIDER-DID "did:web:etzhayyim.com:actor:iyashi")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["care-hours-ack-supports-ko-and-mago"
   "not-a-happiness-score"
   "care-delivery-not-invoked"
   "wellbecoming-substrate"])

(defn- assert-intent! [intent]
  (when-not (= "care-iyashi" (:rail-kind intent))
    (throw (ex-info "iyashi only receives care-iyashi rails" {:rail (:rail-kind intent)})))
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
  "Acknowledge a dry care provisioning intent. Returns receive package (live=false)."
  [intent & {:keys [subject-did]}]
  (assert-intent! intent)
  (let [pkg {:phase :dry-ack
             :provider-did PROVIDER-DID
             :rail-kind "care-iyashi"
             :alloc-id (:alloc-id intent)
             :subject-did subject-did
             :imputed-usd-micros-yr (:imputed-usd-micros-yr intent)
             :received-at-offline true
             :care-delivery-invoked false
             :published false
             :live false
             :cash-usd-micros 0
             :server-held-key false
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :score-surface []
             :note "iyashi dry-ack only — care delivery R2 not invoked"}]
    (pp/assert-no-public-scores! pkg)
    pkg))

(defn receive-from-r1-package
  [r1-pkg]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot receive refused package" r1-pkg)))
  (dry-receive (:intent r1-pkg) :subject-did (:subject-did r1-pkg)))

(defn gated-receive-plan
  "After gated-live-plan authorization, still only dry-ack (no care delivery)."
  [r1-pkg gate & {:keys [env]}]
  (let [plan (care/gated-live-plan r1-pkg gate :env env)
        ack (dry-receive (:intent plan) :subject-did (:subject-did plan))]
    (assoc ack
           :phase :gated-ack-plan
           :authorized-to-publish (boolean (:authorized-to-publish plan))
           :live false
           :care-delivery-invoked false
           :published false
           :note "gated capability presented; iyashi care delivery still not invoked")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
