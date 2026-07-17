(ns fuchi.methods.rail-tooling-okaimono
  "rail_tooling_okaimono.cljc — tooling-okaimono single-rail R1 → gated-live DESIGN.

  Vocation/tool access via okaimono 御買物 for displaced labor wellbecoming (not a score).
  live=false; cash≡0; disclosure held → refuse. Portable .cljc."
  (:require [fuchi.methods.provision :as provision]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def RAIL-KIND "tooling-okaimono")
(def PROVIDER-DID "did:web:etzhayyim.com:actor:okaimono")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["tool-access-supports-household-vocation-recovery"
   "not-a-happiness-score"
   "tooling-is-wellbecoming-substrate"
   "fulfillment-not-invoked"])

#?(:clj
   (defn load-design []
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))]
       (edn/load-edn (io/file actor "data" "rail-tooling-okaimono-design.edn")))))

(defn- assert-invariants! [m]
  (when-not (= 0 (or (:cash-usd-micros m) 0))
    (throw (ex-info "cash≡0" m)))
  (when (or (:server-held-key m) (:published m) (:live m))
    (throw (ex-info "G9/G10" m)))
  true)

(defn tooling-rail [imputed]
  {:kind RAIL-KIND :imputed-usd-micros-yr (long imputed) :member-principal false})

(defn r1-dry-intent [alloc-id imputed]
  (let [i (first (provision/provision [(tooling-rail imputed)] alloc-id))]
    (when-not (= RAIL-KIND (:rail-kind i))
      (throw (ex-info "expected tooling-okaimono" {:got i})))
    (assert-invariants! i)
    (when-not (= PROVIDER-DID (:provider-did i))
      (throw (ex-info "provider must be okaimono" {:p (:provider-did i)})))
    i))

(defn- disclosure-state [person hold-machine]
  (cond
    hold-machine (name (:state hold-machine))
    (pp/exit-suspended? person) "exit-suspended"
    (and (pp/public-person? person) (not (pp/disclosure-ok? person))) "held"
    :else "open"))

(defn refuse-package [alloc-id subject-did imputed reason ds pp?]
  {:alloc-id alloc-id :subject-did subject-did :rail-kind RAIL-KIND
   :provider-did PROVIDER-DID :imputed-usd-micros-yr (long imputed)
   :phase :refused :refusal-reason reason :disclosure-state ds
   :public-person? pp? :priority-stack PRIORITY-STACK
   :multi-gen-facts MULTI-GEN-FACTS :authorized-to-publish false
   :published false :cash-usd-micros 0 :server-held-key false :live false
   :score-surface []})

(defn r1-dry-package
  [{:keys [alloc-id subject-did imputed-usd-micros-yr person hold-machine]
    :or {alloc-id "alloc-tooling" imputed-usd-micros-yr 0}}]
  (let [person (or person {:did subject-did :covenant "vowed"
                           :rails [{:kind "tooling" :active? true}]
                           :floor-usd-micros-yr imputed-usd-micros-yr
                           :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                                        :wellbecoming-attest-fact :submitted
                                        :related-party-edges [] :rider-s2-self-report :none}
                           :exit-suspended? false})
        ds (disclosure-state person hold-machine)
        pp? (pp/public-person? person)
        hold? (or (= ds "held") (= ds "exit-suspended")
                  (and hold-machine (:entitlements-held? hold-machine)))]
    (if hold?
      (refuse-package alloc-id (or subject-did (:did person)) imputed-usd-micros-yr
                      (str "disclosure/entitlements not open (" ds ")") ds pp?)
      (let [intent (r1-dry-intent alloc-id imputed-usd-micros-yr)
            pkg {:alloc-id alloc-id :subject-did (or subject-did (:did person))
                 :rail-kind RAIL-KIND :provider-did PROVIDER-DID
                 :imputed-usd-micros-yr (long imputed-usd-micros-yr)
                 :phase :R1-dry :intent intent :disclosure-state ds
                 :public-person? pp? :priority-stack PRIORITY-STACK
                 :multi-gen-facts MULTI-GEN-FACTS :authorized-to-publish false
                 :published false :cash-usd-micros 0 :server-held-key false
                 :live false :score-surface []}]
        (assert-invariants! pkg)
        (pp/assert-no-public-scores! pkg)
        pkg))))

(defn gated-live-plan [r1-pkg gate & {:keys [env hold-machine]}]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot plan live on refused package" r1-pkg)))
  (let [ds (:disclosure-state r1-pkg)
        hold? (or (= ds "held") (= ds "exit-suspended")
                  (and hold-machine (#{:held :exit-suspended} (:state hold-machine))))]
    (when hold? (throw (ex-info (str "refuse gated-live: " ds) {})))
    (live-gate/require-gate gate env)
    (let [authorized (first (provision/dispatch-live [(:intent r1-pkg)] gate :env env))
          pkg (assoc r1-pkg :phase :gated-live-plan :authorized-to-publish true
                     :authorization authorized :published false :live false
                     :cash-usd-micros 0 :server-held-key false
                     :note "authorized plan only — okaimono fulfillment not invoked")]
      (when (or (:published pkg) (:live pkg)) (throw (ex-info "G10" {})))
      (pp/assert-no-public-scores! pkg)
      pkg)))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))

(defn gated-live-status
  "Non-raising R1→gated-live DESIGN status for tooling-okaimono (vocation recovery).
   Default gate/env refuses. Never fulfills tools; cash≡0; live false."
  [r1-pkg & {:keys [gate env hold-machine]}]
  (cond
    (nil? r1-pkg)
    nil

    (= :refused (:phase r1-pkg))
    {:rail-kind RAIL-KIND
     :provider-did PROVIDER-DID
     :phase :refused
     :r1-phase :refused
     :admissible false
     :authorized-to-publish false
     :fulfillment-executed false
     :refusal-reason (or (:refusal-reason r1-pkg) "r1 refused")
     :disclosure-state (or (:disclosure-state r1-pkg) "n/a")
     :live false
     :cash-usd-micros 0
     :score-surface []
     :priority-stack PRIORITY-STACK
     :note "R1 refused — gated-live not attempted"}

    :else
    (let [g (or gate (live-gate/make-live-gate {:leg "provision"}))
          e (or env {})]
      (try
        (let [plan (gated-live-plan r1-pkg g :env e :hold-machine hold-machine)
              out {:rail-kind RAIL-KIND
                   :provider-did PROVIDER-DID
                   :phase :gated-live-plan
                   :r1-phase (:phase r1-pkg)
                   :admissible true
                   :authorized-to-publish (boolean (:authorized-to-publish plan))
                   :fulfillment-executed false
                   :published false
                   :disclosure-state (or (:disclosure-state r1-pkg) "open")
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :note "gated-live plan authorized — okaimono fulfillment not invoked"}]
          (pp/assert-no-public-scores! out)
          out)
        (catch #?(:clj Exception :cljs :default) ex
          (let [st (live-gate/gate-status g e)
                out {:rail-kind RAIL-KIND
                     :provider-did PROVIDER-DID
                     :phase :refused
                     :r1-phase (:phase r1-pkg)
                     :admissible false
                     :authorized-to-publish false
                     :fulfillment-executed false
                     :refusal-reason (or (ex-message ex)
                                         (get st "reason")
                                         "live gate default refuse")
                     :disclosure-state (or (:disclosure-state r1-pkg) "open")
                     :gate-admissible (boolean (get st "admissible"))
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK
                     :note "R1 dry ok; gated-live refused by default membrane"}]
            (pp/assert-no-public-scores! out)
            out))))))
