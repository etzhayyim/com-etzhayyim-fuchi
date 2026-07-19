(ns fuchi.methods.r2-execute
  "r2_execute.cljc — R2 live-execute membrane for produce/generate/grant/loan.

  Default REFUSE (G10). Even when live_gate is presented and admissible, this
  scaffold returns a gated execute PLAN only — produce-executed / grant-executed /
  loan-executed / quota-executed stay false. No cash. No scores.

  Legs:
    produce  — food-mitsuho / care-iyashi / tooling-okaimono floors
    generate — energy-hikari
    grant    — housing-commons
    quota    — compute-murakumo
    loan     — liquidity-warifu (member-principal; fuchi never creditor cash)

  Portable .cljc."
  (:require [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def EXECUTE-LEGS
  {"produce" "provision"
   "generate" "provision"
   "grant" "provision"
   "quota" "provision"
   "loan" "provision"})

(defn default-refuse-status
  "Bare gate status for provision (default not admissible)."
  []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))

(defn- assert-plan-shape! [plan]
  (when-not (map? plan)
    (throw (ex-info "execute requires a dry produce/receive plan map" {:plan plan})))
  (when-not (= 0 (or (:cash-usd-micros plan) 0))
    (throw (ex-info "cash≡0" plan)))
  (when (true? (:server-held-key plan))
    (throw (ex-info "no-server-key" plan)))
  (when (true? (:published plan))
    (throw (ex-info "G10: cannot execute already-published plan in scaffold" plan)))
  true)

(defn execute-plan
  "Attempt R2 execute. Without admissible live_gate → LiveGateRefused.
   With gate → still only :gated-execute-plan (executed=false)."
  [leg plan gate & {:keys [env]}]
  (when-not (contains? EXECUTE-LEGS leg)
    (throw (ex-info (str "unknown execute leg " leg) {:leg leg})))
  (assert-plan-shape! plan)
  (let [provision-gate (if (= "provision" (:leg gate))
                         gate
                         (live-gate/make-live-gate
                          (assoc gate :leg "provision")))]
    (live-gate/require-gate provision-gate env)
    (let [out {:phase :gated-execute-plan
               :execute-leg leg
               :rail-kind (:rail-kind plan)
               :alloc-id (:alloc-id plan)
               :subject-did (:subject-did plan)
               :provider-did (:provider-did plan)
               :imputed-usd-micros-yr (:imputed-usd-micros-yr plan)
               :authorized-to-execute true
               :produce-executed false
               :generate-executed false
               :land-grant-executed false
               :quota-executed false
               :loan-executed false
               :fulfillment-executed false
               :care-delivery-executed false
               :executed false
               :published false
               :live false
               :cash-usd-micros 0
               :server-held-key false
               :member-principal (boolean (:member-principal plan))
               :priority-stack PRIORITY-STACK
               :score-surface []
               :source-plan-phase (:phase plan)
               :note (str "R2 gated execute PLAN only — " leg
                          " not performed; live scaffold refuses real side-effects")}]
      (pp/assert-no-public-scores! out)
      out)))

(defn refuse-without-gate
  "Document default refuse for a plan (no exception) — status map for reports."
  [leg plan]
  (let [st (default-refuse-status)
        out {:phase :refused
             :execute-leg leg
             :rail-kind (:rail-kind plan)
             :alloc-id (:alloc-id plan)
             :subject-did (:subject-did plan)
             :admissible (boolean (get st "admissible"))
             :refusal-reason (or (get st "reason") "live gate default refuse")
             :executed false
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! out)
    out))

(defn execute-or-refuse
  "If gate+env admissible → gated-execute-plan; else refuse map (no throw)."
  [leg plan gate & {:keys [env]}]
  (try
    (execute-plan leg plan gate :env env)
    (catch Exception e
      (if (= live-gate/live-gate-refused (:fuchi.methods.live-gate/kind (ex-data e)))
        (assoc (refuse-without-gate leg plan)
               :refusal-reason (.getMessage e)
               :admissible false)
        (throw e)))))
