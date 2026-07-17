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
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.displacement-book :as dbook]
            [fuchi.methods.r2-execute :as r2]))

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

(defn council-ratify-plan
  "Offline Council ratification PLAN for housing under council-lv7.
   Lifts housing in rail-flow facts only — does NOT grant land or go live."
  [gov-subject]
  (when-not (= "council-lv7" (:route gov-subject))
    (throw (ex-info "council-ratify-plan requires council-lv7 route" {:route (:route gov-subject)})))
  (let [rails (or (:flow-rails gov-subject) [])
        held (or (:held-rails gov-subject) [])
        new-flow (merge (or (:rail-flow gov-subject) {})
                        (into {} (map (fn [r] [r true]) held)))
        out {:phase :council-ratify-plan
             :subject-did (:subject-did gov-subject)
             :route "council-lv7"
             :authorized-to-release-housing true
             :housing-released-offline true
             :rail-flow-after new-flow
             :land-grant-executed false
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "Council ratify PLAN only — land grant not executed; live refuse"}]
    (pp/assert-no-public-scores! out)
    out))

(defn r2-status-for-rails
  "R2 refuse map per short rail kind (held → refused without gate)."
  [subject rail-flow]
  (let [pkgs (or (get-in subject [:stage-sustenance :packages]) {})]
    (into {}
          (map (fn [[kind v]]
                 (let [plan (:plan v)
                       ok? (true? (get rail-flow kind))]
                   [kind (if (and plan ok?)
                           (r2/refuse-without-gate
                            (or (:execute-leg v) "produce") plan)
                           {:phase :refused
                            :execute-leg kind
                            :reason (if plan "rail-held-by-gov" "no-plan")
                            :executed false
                            :live false
                            :cash-usd-micros 0
                            :score-surface []})]))
               pkgs))))

(defn package-subject
  "Route subject + partial hold + flowable-only rebook + per-rail r2 refuse facts."
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
        flowable-book
        (when-let [sp (:stage-sustenance subject)]
          (dbook/book-flowable
           (:subject-did subject) sp (:rail-flow hold)
           :alloc-id (str "flow-" (:cohort-id subject) "-" (:subject-did subject))))
        r2-map (r2-status-for-rails subject (:rail-flow hold))
        out (merge r hold {:gov-package pkg
                           :flowable-booking flowable-book
                           :r2-by-rail r2-map
                           :live false
                           :cash-usd-micros 0
                           :score-surface []
                           :priority-stack PRIORITY-STACK})]
    (pp/assert-no-public-scores! (dissoc out :gov-package :rail-flow :r2-by-rail :flowable-booking))
    out))

(defn apply-council-ratify-rebook
  "For council-lv7 gov subjects: offline ratify plan + rebook with housing included
   in facts (still land-grant-executed=false, write_live refuse)."
  [gov-subject subject]
  (if-not (= "council-lv7" (:route gov-subject))
    gov-subject
    (let [plan (council-ratify-plan gov-subject)
          sp (:stage-sustenance subject)
          rebook (when sp
                   (dbook/book-flowable
                    (:subject-did subject) sp (:rail-flow-after plan)
                    :alloc-id (str "ratify-" (:cohort-id subject) "-"
                                   (:subject-did subject))
                    :note "post-council-ratify offline book — land grant still not executed"))]
      (assoc gov-subject
             :council-ratify plan
             :post-ratify-booking rebook
             :post-ratify-includes-housing
             (boolean (some #(= "housing-commons" %) (or (:rails rebook) [])))
             :land-grant-executed false
             :live false
             :cash-usd-micros 0))))

(defn package-cohort
  "Attach gov packages + optional council ratify rebook for multi-gen housing path."
  [pkg & {:keys [apply-ratify?] :or {apply-ratify? true}}]
  (if-not (= :offline-enrolled (:phase pkg))
    (assoc pkg :gov nil)
    (let [subjects (:subjects pkg)
          govs0 (mapv package-subject subjects)
          govs (if apply-ratify?
                 (mapv (fn [g s] (apply-council-ratify-rebook g s))
                       govs0 subjects)
                 govs0)
          routes (frequencies (map :route govs))
          held (count (filter :entitlements-held? govs))
          flow (count (filter :may-flow? govs))
          substrate (count (filter :may-flow-substrate? govs))
          flowable-total (reduce + 0 (map #(or (get-in % [:flowable-booking :in-kind-total-usd-micros]) 0) govs))
          post-ratify-total (reduce + 0 (map #(or (get-in % [:post-ratify-booking :in-kind-total-usd-micros]) 0) govs))
          out (assoc pkg
                     :gov-subjects govs
                     :gov-route-counts routes
                     :gov-entitlements-held held
                     :gov-entitlements-may-flow flow
                     :gov-substrate-may-flow substrate
                     :gov-flowable-committed-usd-micros flowable-total
                     :gov-post-ratify-committed-usd-micros post-ratify-total
                     :gov-live false
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK)]
      out)))

(defn package-batch
  "Map package-cohort over a displacement batch."
  [batch & opts]
  (let [pkgs (mapv #(apply package-cohort % opts) (:packages batch))
        all-routes (apply merge-with + (map #(or (:gov-route-counts %) {}) pkgs))
        flowable (reduce + 0 (map #(or (:gov-flowable-committed-usd-micros %) 0) pkgs))
        post (reduce + 0 (map #(or (:gov-post-ratify-committed-usd-micros %) 0) pkgs))]
    (assoc batch
           :packages pkgs
           :gov-route-counts all-routes
           :gov-flowable-committed-usd-micros flowable
           :gov-post-ratify-committed-usd-micros post
           :live false
           :cash-usd-micros 0
           :score-surface []
           :priority-stack PRIORITY-STACK)))
