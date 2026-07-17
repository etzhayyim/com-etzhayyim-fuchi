(ns fuchi.methods.rail-mitsuho
  "rail_mitsuho.cljc — food-mitsuho single-rail R1 → gated-live DESIGN.

  First in-kind SS rail slice (priority gap #3): staple food via mitsuho 瑞穂.
  - R1 dry-run: provisioning intent (published=false, cash≡0)
  - gated-live-plan: live_gate + disclosure open → authorize plan only
  - NEVER calls mitsuho produce API; live flag stays false
  - disclosure held / exit-suspended → refuse
  - no personal scores

  Priority facts: wellbecoming > mago > ko > present (food as multi-gen substrate).
  Portable .cljc."
  (:require [clojure.string :as str]
            [fuchi.methods.provision :as provision]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def RAIL-KIND "food-mitsuho")
(def PROVIDER-DID "did:web:etzhayyim.com:actor:mitsuho")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["staple-kcal-floor-supports-caregiver-and-child-households"
   "not-a-happiness-score"
   "imputed-usd-is-accounting-fact-only"])

#?(:clj
   (defn load-design
     "Load data/rail-mitsuho-design.edn (string-keyed)."
     []
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))]
       (edn/load-edn (io/file actor "data" "rail-mitsuho-design.edn")))))

(defn- assert-invariants! [m]
  (when-not (= 0 (or (:cash-usd-micros m) (:cashUsdMicros m) 0))
    (throw (ex-info "cash≡0 (G2)" m)))
  (when (or (true? (:server-held-key m)) (true? (:serverHeldKey m)))
    (throw (ex-info "no-server-key (G9)" m)))
  (when (or (true? (:published m)) (true? (:live m)))
    (throw (ex-info "G10: rail-mitsuho scaffold never publishes or goes live" m)))
  true)

(defn food-rail
  "Build a single food-mitsuho route-shaped rail map."
  [imputed-usd-micros-yr]
  {:kind RAIL-KIND
   :imputed-usd-micros-yr (long imputed-usd-micros-yr)
   :member-principal false})

(defn r1-dry-intent
  "R1 dry-run provisioning intent for food-mitsuho only."
  [alloc-id imputed-usd-micros-yr]
  (let [rails [(food-rail imputed-usd-micros-yr)]
        intents (provision/provision rails alloc-id)
        i (first intents)]
    (when-not (= RAIL-KIND (:rail-kind i))
      (throw (ex-info "expected food-mitsuho intent" {:got i})))
    (assert-invariants! i)
    (when-not (= PROVIDER-DID (:provider-did i))
      (throw (ex-info "provider must be mitsuho actor DID" {:provider (:provider-did i)})))
    i))

(defn- disclosure-state [person hold-machine]
  (cond
    hold-machine (name (:state hold-machine))
    (pp/exit-suspended? person) "exit-suspended"
    (and (pp/public-person? person) (not (pp/disclosure-ok? person))) "held"
    :else "open"))

(defn refuse-package
  [alloc-id subject-did imputed reason disclosure-state public-person?]
  {:alloc-id alloc-id
   :subject-did subject-did
   :rail-kind RAIL-KIND
   :provider-did PROVIDER-DID
   :imputed-usd-micros-yr (long imputed)
   :phase :refused
   :refusal-reason reason
   :disclosure-state disclosure-state
   :public-person? public-person?
   :priority-stack PRIORITY-STACK
   :multi-gen-facts MULTI-GEN-FACTS
   :authorized-to-publish false
   :published false
   :cash-usd-micros 0
   :server-held-key false
   :live false
   :score-surface []})

(defn r1-dry-package
  "Full R1 dry package for a subject (public-person aware)."
  [{:keys [alloc-id subject-did imputed-usd-micros-yr person hold-machine]
    :or {alloc-id "alloc-food" imputed-usd-micros-yr 0}}]
  (let [person (or person {:did subject-did :covenant "vowed"
                           :rails [{:kind "food" :active? true}]
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
                      (str "disclosure/entitlements not open (" ds ")")
                      ds pp?)
      (let [intent (r1-dry-intent alloc-id imputed-usd-micros-yr)
            pkg {:alloc-id alloc-id
                 :subject-did (or subject-did (:did person))
                 :rail-kind RAIL-KIND
                 :provider-did PROVIDER-DID
                 :imputed-usd-micros-yr (long imputed-usd-micros-yr)
                 :phase :R1-dry
                 :intent intent
                 :disclosure-state ds
                 :public-person? pp?
                 :priority-stack PRIORITY-STACK
                 :multi-gen-facts MULTI-GEN-FACTS
                 :authorized-to-publish false
                 :published false
                 :cash-usd-micros 0
                 :server-held-key false
                 :live false
                 :score-surface []}]
        (assert-invariants! pkg)
        (pp/assert-no-public-scores! pkg)
        pkg))))

(defn gated-live-plan
  "Design-only gated live plan. Requires live_gate + disclosure open.
  On success: phase :gated-live-plan, authorized-to-publish true, live still false.
  Does NOT call mitsuho. Default env/gate refuses."
  [r1-pkg gate & {:keys [env hold-machine]}]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot plan live on refused R1 package" r1-pkg)))
  (let [ds (:disclosure-state r1-pkg)
        hold? (or (= ds "held") (= ds "exit-suspended")
                  (and hold-machine (#{:held :exit-suspended} (:state hold-machine))))]
    (when hold?
      (throw (ex-info (str "refuse gated-live: disclosure " ds)
                      {:kind ::disclosure-hold :package r1-pkg})))
    ;; live_gate raises if not fully satisfied — default refuse
    (live-gate/require-gate gate env)
    (let [intent (:intent r1-pkg)
          authorized (first (provision/dispatch-live [intent] gate :env env))
          pkg (assoc r1-pkg
                     :phase :gated-live-plan
                     :authorized-to-publish true
                     :authorization authorized
                     :published false
                     :live false
                     :cash-usd-micros 0
                     :server-held-key false
                     :note "authorized plan only — mitsuho produce not invoked")]
      (when-not (= 0 (:cash-usd-micros pkg))
        (throw (ex-info "cash≡0" {})))
      (when (or (:published pkg) (:live pkg))
        (throw (ex-info "G10 scaffold: published/live must stay false" {})))
      (pp/assert-no-public-scores! pkg)
      pkg)))

(defn default-refuse-status
  "Non-raising: shows that bare gate+empty env is not admissible for provision."
  []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))

(defn gated-live-status
  "Non-raising R1→gated-live DESIGN status for food-mitsuho.
   Default gate/env refuses. Never executes produce; cash≡0; live false."
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
     :produce-executed false
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
                   :produce-executed false
                   :published false
                   :disclosure-state (or (:disclosure-state r1-pkg) "open")
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :note "gated-live plan authorized — mitsuho produce not invoked"}]
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
                     :produce-executed false
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

(defn lexicon-record
  "Shape package for com.etzhayyim.fuchi.mitsuhoRailDispatch."
  [pkg]
  {:allocId (:alloc-id pkg)
   :subjectDid (:subject-did pkg)
   :railKind RAIL-KIND
   :providerDid PROVIDER-DID
   :imputedUsdMicrosYr (:imputed-usd-micros-yr pkg)
   :phase (name (:phase pkg))
   :refusalReason (or (:refusal-reason pkg) "")
   :disclosureState (or (:disclosure-state pkg) "n/a")
   :publicPerson (boolean (:public-person? pkg))
   :priorityStack (mapv name PRIORITY-STACK)
   :multiGenFacts MULTI-GEN-FACTS
   :authorizedToPublish (boolean (:authorized-to-publish pkg))
   :published false
   :cashUsdMicros 0
   :serverHeldKey false
   :live false
   :scoreSurface []})
