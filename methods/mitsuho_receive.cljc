(ns fuchi.methods.mitsuho-receive
  "mitsuho_receive.cljc — offline dry-run of mitsuho 瑞穂 accepting a food provisioning intent.

  Actor-side counterpart to rail_mitsuho (fuchi plan). Does NOT produce food, does NOT
  go live, does NOT move cash. Acknowledges intent shape + multi-gen floor facts.
  Portable .cljc."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.provision :as provision]))

(def PROVIDER-DID "did:web:etzhayyim.com:actor:mitsuho")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["staple-receive-ack-supports-household-care"
   "not-a-happiness-score"
   "produce-not-invoked"])

(defn- assert-intent! [intent]
  (when-not (= "food-mitsuho" (:rail-kind intent))
    (throw (ex-info "mitsuho only receives food-mitsuho rails" {:rail (:rail-kind intent)})))
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
  "Acknowledge a dry provisioning intent. Returns receive package (live=false)."
  [intent & {:keys [subject-did]}]
  (assert-intent! intent)
  (let [pkg {:phase :dry-ack
             :provider-did PROVIDER-DID
             :rail-kind "food-mitsuho"
             :alloc-id (:alloc-id intent)
             :subject-did subject-did
             :imputed-usd-micros-yr (:imputed-usd-micros-yr intent)
             :received-at-offline true
             :produce-invoked false
             :published false
             :live false
             :cash-usd-micros 0
             :server-held-key false
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :score-surface []
             :note "mitsuho dry-ack only — staple produce R2 not invoked"}]
    (pp/assert-no-public-scores! pkg)
    pkg))

(defn receive-from-r1-package
  "If R1 package is not refused, dry-receive its intent."
  [r1-pkg]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot receive refused package" r1-pkg)))
  (dry-receive (:intent r1-pkg) :subject-did (:subject-did r1-pkg)))

(defn gated-receive-plan
  "After gated-live-plan authorization, still only dry-ack (no produce).
  Requires same live_gate capability as provision; live/produce stay false."
  [r1-pkg gate & {:keys [env]}]
  (let [plan (mitsuho/gated-live-plan r1-pkg gate :env env)
        ack (dry-receive (:intent plan) :subject-did (:subject-did plan))]
    (assoc ack
           :phase :gated-ack-plan
           :authorized-to-publish (boolean (:authorized-to-publish plan))
           :live false
           :produce-invoked false
           :published false
           :note "gated capability presented; mitsuho produce still not invoked")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
