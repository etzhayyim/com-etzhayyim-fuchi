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
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.displacement-book :as dbook]
            [fuchi.methods.displacement-couple :as dcouple]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
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

(defn- disclosure-held-subject?
  "True when disclosure continuity freezes entitlement flow (public-person may stay)."
  [subject]
  (or (true? (:disclosure-held? subject))
      (false? (:entitlements-may-flow? subject))
      (when-let [m (:disclosure-hold subject)]
        (not (disc/entitlements-may-flow? m)))
      (true? (get-in subject [:disclosure-hold :entitlements-held?]))))

(defn package-subject
  "Route subject + partial hold + flowable-only rebook + per-rail r2 refuse facts.
   Disclosure-held freezes ALL rails (stronger than council housing-only hold)."
  [subject]
  (let [r (route-subject subject)
        rails (or (get-in subject [:stage-sustenance :rails]) [])
        disc-held? (disclosure-held-subject? subject)
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
        hold0 (entitlement-hold-for-route (:route r) rails)
        hold (if disc-held?
               (let [all-held (entitlement-hold-for-route "refused" rails)]
                 (assoc all-held
                        :disclosure-held? true
                        :entitlement-hold-reason "disclosure-held"
                        :may-flow? false
                        :may-flow-substrate? false
                        :gov-route-hold hold0))
               (assoc hold0 :disclosure-held? false))
        flowable-book
        (when-let [sp (:stage-sustenance subject)]
          (if disc-held?
            {:phase :held-offline
             :subject-did (:subject-did subject)
             :rails []
             :in-kind-total-usd-micros 0
             :entry-count 0
             :note "disclosure-held — flowable book empty; floors remain plan-only"
             :live false
             :cash-usd-micros 0
             :score-surface []}
            (dbook/book-flowable
             (:subject-did subject) sp (:rail-flow hold)
             :alloc-id (str "flow-" (:cohort-id subject) "-" (:subject-did subject)))))
        r2-map (r2-status-for-rails subject (:rail-flow hold))
        out (merge r hold {:gov-package pkg
                           :flowable-booking flowable-book
                           :r2-by-rail r2-map
                           :disclosure-held? disc-held?
                           :live false
                           :cash-usd-micros 0
                           :score-surface []
                           :priority-stack PRIORITY-STACK})]
    (pp/assert-no-public-scores! (dissoc out :gov-package :rail-flow :r2-by-rail :flowable-booking :gov-route-hold))
    out))

(defn apply-council-ratify-rebook
  "For council-lv7 gov subjects: offline ratify plan + rebook with housing included
   in facts (still land-grant-executed=false, write_live refuse).
   Disclosure-held subjects get empty post-ratify flowable book (housing still not granted live)."
  [gov-subject subject]
  (if-not (= "council-lv7" (:route gov-subject))
    gov-subject
    (let [plan (council-ratify-plan gov-subject)
          disc-held? (or (true? (:disclosure-held? gov-subject))
                         (disclosure-held-subject? subject))
          sp (:stage-sustenance subject)
          rebook (when sp
                   (if disc-held?
                     {:phase :held-offline
                      :subject-did (:subject-did subject)
                      :rails []
                      :in-kind-total-usd-micros 0
                      :entry-count 0
                      :note "disclosure-held — post-ratify flowable empty; land-grant false"
                      :live false
                      :cash-usd-micros 0
                      :score-surface []}
                     (dbook/book-flowable
                      (:subject-did subject) sp (:rail-flow-after plan)
                      :alloc-id (str "ratify-" (:cohort-id subject) "-"
                                     (:subject-did subject))
                      :note "post-council-ratify offline book — land grant still not executed")))]
      (assoc gov-subject
             :council-ratify plan
             :post-ratify-booking rebook
             :post-ratify-includes-housing
             (boolean (and (not disc-held?)
                           (some #(= "housing-commons" %) (or (:rails rebook) []))))
             :disclosure-held? disc-held?
             :land-grant-executed false
             :live false
             :cash-usd-micros 0))))

(defn- event-from-pkg
  "Rebuild displacement event facts for G2 re-eval after gov packaging."
  [pkg]
  (let [ear (:earmark pkg)
        c (:couple pkg)]
    (couple/make-displacement-event
     {:displacing-actor (:displacing-actor pkg)
      :cohort-id (:cohort-id pkg)
      :displaced-count (long (or (get-in pkg [:slots-plan :displaced-count])
                                 (count (:subjects pkg))
                                 0))
      :surplus-usd-micros-yr (long (or (:gross-usd-micros-yr ear)
                                       (:gross-usd-micros-yr c)
                                       0))
      :funded (boolean (or (:funded ear) (:funded c)))})))

(defn- food-pkg-of
  [subject]
  (or (:food-package subject)
      (get-in subject [:stage-sustenance :packages "food" :package])))

(defn- energy-pkg-of
  [subject]
  (or (:energy-package subject)
      (get-in subject [:stage-sustenance :packages "energy" :package])))

(defn- care-pkg-of
  [subject]
  (or (:care-package subject)
      (get-in subject [:stage-sustenance :packages "care" :package])))

(defn- housing-pkg-of
  [subject]
  (or (:housing-package subject)
      (get-in subject [:stage-sustenance :packages "housing" :package])))

(defn- housing-held-by-gov?
  "True when gov partial hold freezes housing (council-lv7 flowable omits housing)."
  [subject]
  (or (false? (get-in subject [:rail-flow "housing"]))
      (contains? (set (or (:held-rails subject) [])) "housing")
      (and (map? (:flowable-booking subject))
           (not (some #{"housing-commons" "housing"}
                      (or (get-in subject [:flowable-booking :rails]) []))))))

(defn attach-substrate-gated-status
  "Multi-gen substrate: care + food + energy + housing R1→gated-live DESIGN status.
   Housing always land-grant-executed=false; Council hold freezes housing gate.
   Default membrane refuses. cash≡0. no scores."
  [subject]
  (let [hold (:disclosure-hold subject)
        food (food-pkg-of subject)
        energy (energy-pkg-of subject)
        care-p (care-pkg-of subject)
        hous (housing-pkg-of subject)
        council-held? (housing-held-by-gov? subject)
        food-st (when food (mitsuho/gated-live-status food :hold-machine hold))
        energy-st (when energy (hikari/gated-live-status energy :hold-machine hold))
        care-st (when care-p (care/gated-live-status care-p :hold-machine hold))
        hous-st (when hous
                  (housing/gated-live-status hous
                                            :hold-machine hold
                                            :council-housing-held? council-held?))
        out (cond-> subject
              food-st (assoc :food-gated-live-status food-st
                             :food-package (or (:food-package subject) food))
              energy-st (assoc :energy-gated-live-status energy-st
                               :energy-package (or (:energy-package subject) energy))
              care-st (assoc :care-gated-live-status care-st
                             :care-package (or (:care-package subject) care-p))
              hous-st (assoc :housing-gated-live-status hous-st
                             :housing-package (or (:housing-package subject) hous)
                             :land-grant-executed false))]
    (when food-st (pp/assert-no-public-scores! food-st))
    (when energy-st (pp/assert-no-public-scores! energy-st))
    (when care-st (pp/assert-no-public-scores! care-st))
    (when hous-st (pp/assert-no-public-scores! hous-st))
    out))

(defn- package-subject-row
  "Route + optional council ratify rebook for one subject (L4 or tenure)."
  [subject apply-ratify?]
  (let [g0 (package-subject subject)]
    (if (and apply-ratify? (= "council-lv7" (:route g0)))
      (apply-council-ratify-rebook g0 subject)
      g0)))

(defn- earmark-from-pkg
  [pkg]
  (or (:earmark pkg)
      (when-let [c (or (:couple pkg) (:tenure-couple pkg))]
        (couple/make-cohort-earmark
         {:cohort-id (:cohort-id pkg)
          :displacing-actor (:displacing-actor pkg)
          :gross-usd-micros-yr (or (:gross-usd-micros-yr c) 0)
          :tithe-usd-micros (or (:tithe-usd-micros c) 0)
          :earmark-usd-micros-yr (or (:earmark-usd-micros-yr c) 0)
          :funded (boolean (:funded c))}))))

(defn- package-subject-list
  "Gov package + flowable couple re-eval for a subject list."
  [pkg subjects apply-ratify? ear pre-couple]
  (if-not (seq subjects)
    {:subjects subjects
     :gov-subjects []
     :couple pre-couple
     :couple-post-ratify nil
     :gov-route-counts {}
     :gov-flowable-committed-usd-micros 0
     :gov-post-ratify-committed-usd-micros 0
     :gov-entitlements-held 0
     :gov-entitlements-may-flow 0
     :gov-substrate-may-flow 0}
    (let [govs (mapv #(package-subject-row % apply-ratify?) subjects)
          routes (frequencies (map :route govs))
          held (count (filter :entitlements-held? govs))
          flow (count (filter :may-flow? govs))
          substrate (count (filter :may-flow-substrate? govs))
          flowable-total (reduce + 0 (map #(or (get-in % [:flowable-booking :in-kind-total-usd-micros]) 0) govs))
          post-ratify-total (reduce + 0 (map #(or (get-in % [:post-ratify-booking :in-kind-total-usd-micros]) 0) govs))
          re (when ear
               (try
                 (dcouple/reevaluate-after-gov (event-from-pkg pkg) ear subjects govs)
                 (catch Exception _ nil)))
          subjects' (mapv attach-substrate-gated-status (or (:subjects re) subjects))]
      {:subjects subjects'
       :gov-subjects govs
       :couple (or (:couple re) pre-couple)
       :couple-post-ratify (:couple-post-ratify re)
       :gov-route-counts routes
       :gov-flowable-committed-usd-micros flowable-total
       :gov-post-ratify-committed-usd-micros post-ratify-total
       :gov-entitlements-held held
       :gov-entitlements-may-flow flow
       :gov-substrate-may-flow substrate})))

(defn package-cohort
  "Attach gov packages for L4 subjects and (if present) L6 tenure subjects.
   Flowable-first couple re-eval on both; post-ratify plan keeps land-grant false."
  [pkg & {:keys [apply-ratify?] :or {apply-ratify? true}}]
  (if-not (= :offline-enrolled (:phase pkg))
    (assoc pkg :gov nil)
    (let [ear (earmark-from-pkg pkg)
          l4 (package-subject-list pkg (:subjects pkg) apply-ratify? ear (:couple pkg))
          ten-subs (or (:tenure-subjects pkg) [])
          ten (when (seq ten-subs)
                (package-subject-list pkg ten-subs apply-ratify? ear (:tenure-couple pkg)))
          ;; L4 routes stay L4-only; tenure routes live under :tenure-gov-route-counts
          out (cond-> (assoc pkg
                             :subjects (:subjects l4)
                             :couple-pre-gov (:couple pkg)
                             :couple (:couple l4)
                             :couple-post-ratify (:couple-post-ratify l4)
                             :gov-subjects (:gov-subjects l4)
                             :gov-route-counts (:gov-route-counts l4)
                             :gov-entitlements-held (:gov-entitlements-held l4)
                             :gov-entitlements-may-flow (:gov-entitlements-may-flow l4)
                             :gov-substrate-may-flow (:gov-substrate-may-flow l4)
                             :gov-flowable-committed-usd-micros
                             (:gov-flowable-committed-usd-micros l4)
                             :gov-post-ratify-committed-usd-micros
                             (:gov-post-ratify-committed-usd-micros l4)
                             :gov-live false
                             :live false
                             :cash-usd-micros 0
                             :score-surface []
                             :priority-stack PRIORITY-STACK)
                ten
                (assoc :tenure-subjects (:subjects ten)
                       :tenure-couple-pre-gov (:tenure-couple pkg)
                       :tenure-couple (:couple ten)
                       :tenure-couple-post-ratify (:couple-post-ratify ten)
                       :tenure-gov-subjects (:gov-subjects ten)
                       :tenure-gov-route-counts (:gov-route-counts ten)
                       :tenure-gov-flowable-committed-usd-micros
                       (:gov-flowable-committed-usd-micros ten)
                       :tenure-gov-post-ratify-committed-usd-micros
                       (:gov-post-ratify-committed-usd-micros ten)
                       :tenure-gov-entitlements-held (:gov-entitlements-held ten)
                       :tenure-gov-entitlements-may-flow (:gov-entitlements-may-flow ten)
                       :tenure-gov-substrate-may-flow (:gov-substrate-may-flow ten)))]
      out)))

(defn package-batch
  "Map package-cohort over a displacement batch. Idempotent if already packaged."
  [batch & opts]
  (if (:gov-packaged? batch)
    batch
    (let [pkgs (mapv #(apply package-cohort % opts) (:packages batch))
          all-routes (apply merge-with + (map #(or (:gov-route-counts %) {}) pkgs))
          flowable (reduce + 0 (map #(or (:gov-flowable-committed-usd-micros %) 0) pkgs))
          post (reduce + 0 (map #(or (:gov-post-ratify-committed-usd-micros %) 0) pkgs))
          ten-flow (reduce + 0 (map #(or (:tenure-gov-flowable-committed-usd-micros %) 0) pkgs))
          ten-post (reduce + 0 (map #(or (:tenure-gov-post-ratify-committed-usd-micros %) 0) pkgs))
          ten-routes (apply merge-with + (map #(or (:tenure-gov-route-counts %) {}) pkgs))]
      (assoc batch
             :packages pkgs
             :gov-route-counts all-routes
             :gov-flowable-committed-usd-micros flowable
             :gov-post-ratify-committed-usd-micros post
             :tenure-gov-route-counts ten-routes
             :tenure-gov-flowable-committed-usd-micros ten-flow
             :tenure-gov-post-ratify-committed-usd-micros ten-post
             :gov-packaged? true
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK))))

(defn force-disclosure-held
  "Offline stress: mark subject disclosure held (entitlements freeze). No scores."
  [subject & {:keys [reason]
              :or {reason "stress-stale-disclosure-all-held"}}]
  (let [hold0 (or (:disclosure-hold subject)
                  {:state :open :entitlements-held? false :history []})
        hold (assoc hold0
                    :state :held
                    :entitlements-held? true
                    :hold-reason reason
                    :public-person? (or (:public-person? hold0) true))
        out (assoc subject
                   :disclosure-hold hold
                   :disclosure-state :held
                   :disclosure-held? true
                   :entitlements-may-flow? false
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores!
     (select-keys out [:disclosure-state :disclosure-held? :entitlements-may-flow?
                       :live :cash-usd-micros :score-surface]))
    out))

(defn package-batch-all-disclosure-held
  "Priority #2 stress: force every enrolled/tenure subject held, re-run G7 package.
   Expectation: gov flowable → 0; land-grant false; live false; G2 may stay admissible
   (funded earmark covers zero committed). cash≡0. no scores."
  [batch & opts]
  (let [pkgs
        (mapv
         (fn [p]
           (if-not (= :offline-enrolled (:phase p))
             (dissoc p :gov-packaged?)
             (assoc p
                    :subjects (mapv force-disclosure-held (or (:subjects p) []))
                    :tenure-subjects (mapv force-disclosure-held
                                           (or (:tenure-subjects p) []))
                    :tenure-disclosure-open 0
                    :tenure-disclosure-held (count (or (:tenure-subjects p) []))
                    :gov-packaged? false)))
         (or (:packages batch) []))
        base (assoc batch
                    :packages pkgs
                    :gov-packaged? false
                    :tenure-disclosure-open 0
                    :tenure-disclosure-held
                    (reduce + 0 (map #(count (or (:tenure-subjects %) [])) pkgs))
                    :disclosure-stress :all-held
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK)
        out (apply package-batch base opts)
        held-n (reduce + 0
                       (map (fn [p]
                              (+ (count (filter :disclosure-held? (:subjects p)))
                                 (count (filter :disclosure-held? (:tenure-subjects p)))))
                            (:packages out)))]
    (assoc out
           :disclosure-stress :all-held
           :disclosure-stress-held-subjects held-n
           :gov-flowable-after-all-held
           (or (:gov-flowable-committed-usd-micros out) 0)
           :tenure-gov-flowable-after-all-held
           (or (:tenure-gov-flowable-committed-usd-micros out) 0)
           :live false
           :cash-usd-micros 0
           :score-surface []
           :priority-stack PRIORITY-STACK
           :note "all-disclosure-held stress — flowable frozen; live refuse; land grant false")))
