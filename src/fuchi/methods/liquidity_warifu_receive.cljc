(ns fuchi.methods.liquidity-warifu-receive
  "liquidity_warifu_receive.cljc — offline dry-run of warifu accepting member-principal residual.

  Does NOT originate loan, does NOT make fuchi a creditor, cash≡0 always.
  Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-liquidity-warifu :as warifu]
            [fuchi.methods.live-gate :as live-gate]))

(def PROVIDER-DID "did:web:etzhayyim.com:actor:warifu")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["member-principal-ack-not-fuchi-cash"
   "qard-hasan-zero-percent"
   "not-a-happiness-score"
   "loan-not-invoked"])

(defn- assert-intent! [intent]
  (when-not (= "liquidity-warifu" (:rail-kind intent))
    (throw (ex-info "warifu only receives liquidity-warifu rails" {:rail (:rail-kind intent)})))
  (when-not (= PROVIDER-DID (:provider-did intent))
    (throw (ex-info "provider DID mismatch" {:provider (:provider-did intent)})))
  (when-not (true? (:member-principal intent))
    (throw (ex-info "liquidity must be member-principal" intent)))
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
             :rail-kind "liquidity-warifu"
             :alloc-id (:alloc-id intent)
             :subject-did subject-did
             :imputed-usd-micros-yr (:imputed-usd-micros-yr intent)
             :received-at-offline true
             :member-principal true
             :loan-invoked false
             :published false
             :live false
             :cash-usd-micros 0
             :server-held-key false
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :score-surface []
             :note "warifu dry-ack only — loan R2 not invoked; fuchi not creditor"}]
    (pp/assert-no-public-scores! pkg)
    pkg))

(defn receive-from-r1-package [r1-pkg]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot receive refused package" r1-pkg)))
  (dry-receive (:intent r1-pkg) :subject-did (:subject-did r1-pkg)))

(defn gated-receive-plan
  [r1-pkg gate & {:keys [env]}]
  (let [plan (warifu/gated-live-plan r1-pkg gate :env env)
        ack (dry-receive (:intent plan) :subject-did (:subject-did plan))]
    (assoc ack
           :phase :gated-ack-plan
           :authorized-to-publish (boolean (:authorized-to-publish plan))
           :live false
           :loan-invoked false
           :member-principal true
           :published false
           :note "gated capability presented; warifu loan still not invoked")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))
