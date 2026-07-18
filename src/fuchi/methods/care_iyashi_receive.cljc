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

(defn gated-receive-status
  "Non-raising R1→gated-receive DESIGN for care-iyashi (孫/子 multi-gen care).
   Default gate/env refuses. Never invokes care delivery; cash≡0; live=false."
  [r1-pkg & {:keys [gate env]}]
  (cond
    (nil? r1-pkg)
    nil

    (= :refused (:phase r1-pkg))
    (let [out {:rail-kind "care-iyashi"
               :provider-did PROVIDER-DID
               :phase :refused
               :r1-phase :refused
               :admissible false
               :authorized-to-publish false
               :care-delivery-invoked false
               :refusal-reason (or (:refusal-reason r1-pkg) "r1 refused")
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :multi-gen-facts MULTI-GEN-FACTS
               :note "R1 refused — gated-receive not attempted"}]
      (pp/assert-no-public-scores! out)
      out)

    :else
    (let [g (or gate (live-gate/make-live-gate {:leg "provision"}))
          e (or env {})]
      (try
        (let [ack (gated-receive-plan r1-pkg g :env e)
              out {:rail-kind "care-iyashi"
                   :provider-did PROVIDER-DID
                   :phase :gated-ack-plan
                   :r1-phase (:phase r1-pkg)
                   :admissible true
                   :authorized-to-publish (boolean (:authorized-to-publish ack))
                   :care-delivery-invoked false
                   :published false
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :multi-gen-facts MULTI-GEN-FACTS
                   :note "gated-receive plan authorized — iyashi care delivery not invoked"}]
          (pp/assert-no-public-scores! out)
          out)
        (catch #?(:clj Exception :cljs :default) ex
          (let [st (live-gate/gate-status g e)
                out {:rail-kind "care-iyashi"
                     :provider-did PROVIDER-DID
                     :phase :refused
                     :r1-phase (:phase r1-pkg)
                     :admissible false
                     :authorized-to-publish false
                     :care-delivery-invoked false
                     :refusal-reason (or (ex-message ex)
                                         (get st "reason")
                                         "live gate default refuse")
                     :gate-admissible (boolean (get st "admissible"))
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK
                     :multi-gen-facts MULTI-GEN-FACTS
                     :note "R1 dry ok; gated-receive refused by default membrane"}]
            (pp/assert-no-public-scores! out)
            out))))))
