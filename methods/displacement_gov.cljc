(ns fuchi.methods.displacement-gov
  "displacement_gov.cljc — offline governance routing for displacement SS packages.

  G7 non-adjudicating route: rider → refused; commons-land/housing → council-lv7;
  above ceiling → sbt-vote; else auto. For sbt-vote, open a dry 1SBT=1vote package
  (timelock 48h); finalize_binding remains refuse-by-default.

  Multi-gen housing (commons) intentionally escalates to Council Lv7 — not auto cash.
  cash≡0. no scores. live=false. Portable .cljc."
  (:require [clojure.string :as str]
            [fuchi.methods.route :as route]
            [fuchi.methods.vote :as vote]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- context-for-subject
  "Text blob for rider/invariant scan (facts about rails, not scores)."
  [subject]
  (let [rails (or (get-in subject [:stage-sustenance :rails]) [])
        kinds (str/join " " rails)]
    (str "displacement cohort " (:cohort-id subject)
         " actor " (:displacing-actor subject)
         " stage " (:stage subject)
         " rails " kinds
         (when (some #{"housing"} rails) " commons-land housing grant multi-gen ko mago")
         " wellbecoming")))

(defn route-subject
  "Pure G7 route for one enrolled subject based on booked in-kind total + rails context."
  [subject]
  (let [committed (long (or (get-in subject [:booking :in-kind-total-usd-micros])
                            (get-in subject [:stage-sustenance :floor-usd-micros-yr])
                            0))
        ctx (context-for-subject subject)
        rider (route/rider-hit ctx)
        inv (route/touches-invariant ctx)
        r (route/gov-route committed inv rider)
        out {:subject-did (:subject-did subject)
             :cohort-id (:cohort-id subject)
             :stage (:stage subject)
             :committed-usd-micros-yr committed
             :route r
             :invariant-touch inv
             :rider (or rider "")
             :ceiling-usd-micros-yr route/OPTIMISTIC-CEILING-USD-MICROS-YR
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "G7 route only — fuchi does not decide"}]
    (pp/assert-no-public-scores! out)
    out))

(defn open-sbt-vote-package
  "Offline vote package for a subject routed to sbt-vote. Does not finalize early."
  [subject-route & {:keys [opened-at now ballots]
                    :or {opened-at 1000 now 1000}}]
  (let [ballots (or ballots
                    [(vote/make-ballot {:voter-did "did:web:etzhayyim.com:member:abel"
                                        :choice "yes" :cast-at opened-at})
                     (vote/make-ballot {:voter-did "did:web:etzhayyim.com:member:seth"
                                        :choice "yes" :cast-at (+ opened-at 1)})
                     (vote/make-ballot {:voter-did "did:web:etzhayyim.com:member:eve"
                                        :choice "yes" :cast-at (+ opened-at 2)})])
        tallied (vote/tally ballots opened-at now)
        live-st (live-gate/gate-status (live-gate/make-live-gate {:leg "vote"}) {})
        out {:phase :vote-open-offline
             :subject-did (:subject-did subject-route)
             :route "sbt-vote"
             :opened-at opened-at
             :now now
             :timelock-h vote/DEFAULT-TIMELOCK-H
             :tally tallied
             :outcome (get tallied "outcome")
             :finalizable (boolean (get tallied "finalizable"))
             :finalize-binding-admissible (boolean (get live-st "admissible"))
             :ballot-count (count ballots)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "offline sbt-vote package — finalize_binding refused by default"}]
    (pp/assert-no-public-scores! out)
    out))

(defn council-pending-package
  "Offline council-lv7 pending package (housing/commons multi-gen)."
  [subject-route]
  (let [out {:phase :council-pending-offline
             :subject-did (:subject-did subject-route)
             :route "council-lv7"
             :reason "invariant-adjacent: commons-land / multi-gen housing"
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "Council Lv7+ required offline plan — no auto grant"}]
    (pp/assert-no-public-scores! out)
    out))

;; wellbecoming > 孫 > 子: substrate may dry-flow under Council; housing grant waits.
(def SUBSTRATE-RAILS #{"care" "food" "energy"})
(def COUNCIL-HELD-RAILS #{"housing"})

(defn rail-may-flow?
  "Per-rail dry-flow permission under a G7 route (facts only; still not live execute)."
  [route rail]
  (case route
    "auto" true
    "council-lv7" (not (contains? COUNCIL-HELD-RAILS rail))
    "sbt-vote" false
    "refused" false
    false))

(defn entitlement-hold-for-route
  "Partial holds: council holds housing only; care/food/energy may-flow (multi-gen substrate).
   sbt-vote/refused hold all. Still offline — no live produce."
  ([route]
   (entitlement-hold-for-route route ["care" "food" "energy" "housing" "tooling" "compute"]))
  ([route rails]
   (let [rails (vec (or rails []))
         flow-map (into {} (map (fn [r] [r (rail-may-flow? route r)]) rails))
         held-rails (vec (filter #(false? (get flow-map %)) rails))
         flow-rails (vec (filter #(true? (get flow-map %)) rails))
         substrate-ok (every? #(or (not (contains? (set rails) %))
                                   (true? (get flow-map %)))
                              SUBSTRATE-RAILS)
         any-held (boolean (seq held-rails))
         any-flow (boolean (seq flow-rails))
         reason (case route
                  "auto" nil
                  "council-lv7" "housing-held-awaiting-council-lv7;substrate-may-flow"
                  "sbt-vote" "awaiting-sbt-vote-timelock"
                  "refused" "gov-refused"
                  "unknown-route")]
     {:entitlements-held? any-held
      :entitlement-hold-reason reason
      :may-flow? any-flow
      :may-flow-substrate? substrate-ok
      :rail-flow flow-map
      :held-rails held-rails
      :flow-rails flow-rails
      :priority-stack PRIORITY-STACK
      :live false
      :cash-usd-micros 0
      :score-surface []})))

(defn package-subject
  "Route subject and attach dry vote/council package + partial entitlement hold facts."
  [subject]
  (let [r (route-subject subject)
        rails (or (get-in subject [:stage-sustenance :rails]) [])
        pkg (case (:route r)
              "sbt-vote" (open-sbt-vote-package r)
              "council-lv7" (council-pending-package r)
              "refused" {:phase :gov-refused :route "refused"
                         :subject-did (:subject-did r)
                         :live false :cash-usd-micros 0 :score-surface []
                         :priority-stack PRIORITY-STACK}
              {:phase :auto-offline :route "auto"
               :subject-did (:subject-did r)
               :live false :cash-usd-micros 0 :score-surface []
               :priority-stack PRIORITY-STACK})
        hold (entitlement-hold-for-route (:route r) rails)
        out (merge r hold {:gov-package pkg
                           :live false
                           :cash-usd-micros 0
                           :score-surface []
                           :priority-stack PRIORITY-STACK})]
    (pp/assert-no-public-scores! (dissoc out :gov-package :rail-flow))
    out))

(defn package-cohort
  "Attach gov packages to all subjects in a displacement cohort package."
  [pkg]
  (if-not (= :offline-enrolled (:phase pkg))
    (assoc pkg :gov nil)
    (let [govs (mapv package-subject (:subjects pkg))
          routes (frequencies (map :route govs))
          held (count (filter :entitlements-held? govs))
          flow (count (filter :may-flow? govs))
          substrate (count (filter :may-flow-substrate? govs))
          out (assoc pkg
                     :gov-subjects govs
                     :gov-route-counts routes
                     :gov-entitlements-held held
                     :gov-entitlements-may-flow flow
                     :gov-substrate-may-flow substrate
                     :gov-live false
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK)]
      out)))

(defn package-batch
  "Map package-cohort over a displacement batch."
  [batch]
  (let [pkgs (mapv package-cohort (:packages batch))
        all-routes (apply merge-with + (map #(or (:gov-route-counts %) {}) pkgs))]
    (assoc batch
           :packages pkgs
           :gov-route-counts all-routes
           :live false
           :cash-usd-micros 0
           :score-surface []
           :priority-stack PRIORITY-STACK)))
