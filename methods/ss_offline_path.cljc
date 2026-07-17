(ns fuchi.methods.ss-offline-path
  "ss_offline_path.cljc — end-to-end OFFLINE path for covenantal SS fragment.

  Priority stack path (offline only):
    (1) L0 enroll scaffold + liberation ladder climb (disclosure-gated, care-first for 孫/子)
    (2) disclosure hold machine + continuity tick (+ optional held stress)
    (3) mitsuho/hikari (and other rails) R1 → gated-live DESIGN status (default refuse)
    + stage_sustenance floors from ladder rails-hint (care/housing first at L4)
    + dry receive / dry produce plans + R2 execute refuse
    + optional itonami displacement facts

  live=false throughout. cash≡0. liquidity is member-principal only. no scores.
  Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.liberation-ladder :as lad]
            [fuchi.methods.stage-sustenance :as stage]
            [fuchi.methods.rail-mitsuho :as food]
            [fuchi.methods.rail-hikari :as energy]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.rail-liquidity-warifu :as liquidity]
            [fuchi.methods.mitsuho-receive :as frecv]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-receive :as hrecv]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.care-iyashi-receive :as crecv]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.tooling-okaimono-receive :as trecv]
            [fuchi.methods.tooling-okaimono-produce-plan :as tprod]
            [fuchi.methods.compute-murakumo-receive :as mrecv]
            [fuchi.methods.compute-murakumo-produce-plan :as cmpprod]
            [fuchi.methods.housing-commons-receive :as housrecv]
            [fuchi.methods.housing-commons-produce-plan :as housprod]
            [fuchi.methods.liquidity-warifu-receive :as wrecv]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.r2-execute :as r2]
            [fuchi.methods.itonami-bridge :as itonami]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def ^:private FRESH-DISC
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted
   :related-party-edges [] :rider-s2-self-report :none})

(def ^:private STALE-DISC
  {:wage-labor-band :stale :state-benefits? false
   :wellbecoming-attest-fact :stale
   :related-party-edges [] :rider-s2-self-report :none})

(defn run-food-path
  "Offline path for one subject across in-kind rails + optional liquidity residual.

  Covers priorities (1) L0 enroll (2) disclosure hold+continuity (3) mitsuho/hikari
  R1→gated-live DESIGN (default refuse) + R2 execute refuse. Never live."
  [{:keys [subject-did vow-text member-signature food-imputed-usd-micros-yr
           energy-imputed-usd-micros-yr care-imputed-usd-micros-yr
           housing-imputed-usd-micros-yr tooling-imputed-usd-micros-yr
           compute-imputed-usd-micros-yr liquidity-imputed-usd-micros-yr
           include-disclosure-stress ladder-target-stage]
    :or {food-imputed-usd-micros-yr 2000000000 energy-imputed-usd-micros-yr 0
         care-imputed-usd-micros-yr 0 housing-imputed-usd-micros-yr 0
         tooling-imputed-usd-micros-yr 0 compute-imputed-usd-micros-yr 0
         liquidity-imputed-usd-micros-yr 0
         include-disclosure-stress true
         ;; L4 = multi-gen care first (wellbecoming > 孫 > 子)
         ladder-target-stage "L4"}}]
  (let [enrolled (l0/enroll {:subject-did subject-did
                             :vow-text (or vow-text "L0 offline path vow")
                             :member-signature (or member-signature (str "sig-" subject-did))
                             :covenant "outreach"})
        person {:did subject-did
                :covenant "vowed"
                :rails (cond-> []
                         (pos? food-imputed-usd-micros-yr) (conj {:kind "food" :active? true})
                         (pos? energy-imputed-usd-micros-yr) (conj {:kind "energy" :active? true})
                         (pos? care-imputed-usd-micros-yr) (conj {:kind "care" :active? true})
                         (pos? housing-imputed-usd-micros-yr) (conj {:kind "housing" :active? true})
                         (pos? tooling-imputed-usd-micros-yr) (conj {:kind "tooling" :active? true})
                         (pos? compute-imputed-usd-micros-yr) (conj {:kind "compute" :active? true})
                         (pos? liquidity-imputed-usd-micros-yr) (conj {:kind "liquidity" :active? true}))
                :floor-usd-micros-yr (+ food-imputed-usd-micros-yr energy-imputed-usd-micros-yr
                                        care-imputed-usd-micros-yr housing-imputed-usd-micros-yr
                                        tooling-imputed-usd-micros-yr compute-imputed-usd-micros-yr
                                        liquidity-imputed-usd-micros-yr)
                :disclosure FRESH-DISC
                :exit-suspended? false
                :stage "L0"}
        hold0 (dh/initial person)
        ;; priority (2): continuity tick keeps open on fresh disclosure
        cont-open (disc/tick hold0 person :disclosure FRESH-DISC :reason "fresh-open")
        hold (:machine cont-open)
        person-open (:person cont-open)
        ;; (1) liberation ladder climb offline while disclosure open (care-first L4)
        ladder-open (lad/climb-to-stage
                     person-open hold ladder-target-stage
                     :member-signature (or member-signature (str "sig-" subject-did)))
        person-ladder (or (:person ladder-open) person-open)
        ladder-hist (or (:history ladder-open) [])
        ladder-last (last ladder-hist)
        ;; stage floors from ladder rails-hint (L4: care → housing first)
        stage-pkg (stage/build-for-stage
                   person-ladder hold
                   :imputed-overrides
                   (cond-> {}
                     (pos? food-imputed-usd-micros-yr) (assoc "food" food-imputed-usd-micros-yr)
                     (pos? energy-imputed-usd-micros-yr) (assoc "energy" energy-imputed-usd-micros-yr)
                     (pos? care-imputed-usd-micros-yr) (assoc "care" care-imputed-usd-micros-yr)
                     (pos? housing-imputed-usd-micros-yr) (assoc "housing" housing-imputed-usd-micros-yr)
                     (pos? tooling-imputed-usd-micros-yr) (assoc "tooling" tooling-imputed-usd-micros-yr)
                     (pos? compute-imputed-usd-micros-yr) (assoc "compute" compute-imputed-usd-micros-yr)))
        stage-row (stage/public-floor-row stage-pkg)
        stage-r2-phases (map (fn [[_ v]] (get-in v [:r2 :phase]))
                             (or (:packages stage-pkg) {}))
        cont-series (when include-disclosure-stress
                      (disc/tick-series person [FRESH-DISC STALE-DISC FRESH-DISC]))
        held-stress (when include-disclosure-stress
                      (let [t (disc/tick hold person :disclosure STALE-DISC
                                         :reason "stale-hold-stress")
                            hm (:machine t)
                            lad-ref (lad/advance-offline person-open hm
                                                         :member-signature "sig-held")]
                        {:action (:action t)
                         :to-state (:to-state t)
                         :held? (:held? t)
                         :entitlements-may-flow?
                         (disc/entitlements-may-flow? hm)
                         :food-r1-when-held
                         (when (pos? food-imputed-usd-micros-yr)
                           (food/r1-dry-package
                            {:alloc-id (str "food-held-" subject-did)
                             :subject-did subject-did
                             :imputed-usd-micros-yr food-imputed-usd-micros-yr
                             :person (assoc person :disclosure STALE-DISC)
                             :hold-machine hm}))
                         :ladder-advance-phase (name (:phase lad-ref))
                         :ladder-refusal-reason (:refusal-reason lad-ref)
                         :live false
                         :cash-usd-micros 0
                         :score-surface []
                         :priority-stack PRIORITY-STACK
                         :note "stress only — open path uses fresh disclosure"}))
        food-pkg (when (pos? food-imputed-usd-micros-yr)
                   (food/r1-dry-package
                    {:alloc-id (str "food-" subject-did)
                     :subject-did subject-did
                     :imputed-usd-micros-yr food-imputed-usd-micros-yr
                     :person person-open
                     :hold-machine hold}))
        energy-pkg (when (pos? energy-imputed-usd-micros-yr)
                     (energy/r1-dry-package
                      {:alloc-id (str "energy-" subject-did)
                       :subject-did subject-did
                       :imputed-usd-micros-yr energy-imputed-usd-micros-yr
                       :person person-open
                       :hold-machine hold}))
        care-pkg (when (pos? care-imputed-usd-micros-yr)
                   (care/r1-dry-package
                    {:alloc-id (str "care-" subject-did)
                     :subject-did subject-did
                     :imputed-usd-micros-yr care-imputed-usd-micros-yr
                     :person person-open
                     :hold-machine hold}))
        housing-pkg (when (pos? housing-imputed-usd-micros-yr)
                      (housing/r1-dry-package
                       {:alloc-id (str "housing-" subject-did)
                        :subject-did subject-did
                        :imputed-usd-micros-yr housing-imputed-usd-micros-yr
                        :person person-open
                        :hold-machine hold}))
        tooling-pkg (when (pos? tooling-imputed-usd-micros-yr)
                      (tooling/r1-dry-package
                       {:alloc-id (str "tooling-" subject-did)
                        :subject-did subject-did
                        :imputed-usd-micros-yr tooling-imputed-usd-micros-yr
                        :person person-open
                        :hold-machine hold}))
        compute-pkg (when (pos? compute-imputed-usd-micros-yr)
                      (compute/r1-dry-package
                       {:alloc-id (str "compute-" subject-did)
                        :subject-did subject-did
                        :imputed-usd-micros-yr compute-imputed-usd-micros-yr
                        :person person-open
                        :hold-machine hold}))
        liquidity-pkg (when (pos? liquidity-imputed-usd-micros-yr)
                        (liquidity/r1-dry-package
                         {:alloc-id (str "liquidity-" subject-did)
                          :subject-did subject-did
                          :imputed-usd-micros-yr liquidity-imputed-usd-micros-yr
                          :person person-open
                          :hold-machine hold}))
        ;; priority (3): all in-kind rails R1 → gated-live DESIGN (default env refuse)
        food-gated (when (and food-pkg (not= :refused (:phase food-pkg)))
                     (food/gated-live-status food-pkg :hold-machine hold))
        energy-gated (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                       (energy/gated-live-status energy-pkg :hold-machine hold))
        care-gated (when (and care-pkg (not= :refused (:phase care-pkg)))
                     (care/gated-live-status care-pkg :hold-machine hold))
        housing-gated (when (and housing-pkg (not= :refused (:phase housing-pkg)))
                        (housing/gated-live-status housing-pkg :hold-machine hold))
        tooling-gated (when (and tooling-pkg (not= :refused (:phase tooling-pkg)))
                        (tooling/gated-live-status tooling-pkg :hold-machine hold))
        compute-gated (when (and compute-pkg (not= :refused (:phase compute-pkg)))
                        (compute/gated-live-status compute-pkg :hold-machine hold))
        liquidity-gated (when (and liquidity-pkg (not= :refused (:phase liquidity-pkg)))
                          (liquidity/gated-live-status liquidity-pkg :hold-machine hold))
        food-plan (when (and food-pkg (not= :refused (:phase food-pkg)))
                    (mprod/plan-from-r1 food-pkg))
        energy-plan (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                      (hprod/plan-from-r1 energy-pkg))
        food-ack (when (and food-pkg (not= :refused (:phase food-pkg)))
                   (frecv/receive-from-r1-package food-pkg))
        ;; priority (3): mitsuho/hikari gated-receive DESIGN (default refuse; produce/generate false)
        food-recv-gated (when (and food-pkg (not= :refused (:phase food-pkg)))
                          (frecv/gated-receive-status food-pkg))
        energy-recv-gated (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                            (hrecv/gated-receive-status energy-pkg))
        care-plan (when (and care-pkg (not= :refused (:phase care-pkg)))
                    (cprod/plan-from-r1 care-pkg))
        care-ack (when (and care-pkg (not= :refused (:phase care-pkg)))
                   (crecv/receive-from-r1-package care-pkg))
        housing-plan (when (and housing-pkg (not= :refused (:phase housing-pkg)))
                       (housprod/plan-from-r1 housing-pkg))
        housing-ack (when (and housing-pkg (not= :refused (:phase housing-pkg)))
                      (housrecv/receive-from-r1-package housing-pkg))
        tooling-plan (when (and tooling-pkg (not= :refused (:phase tooling-pkg)))
                       (tprod/plan-from-r1 tooling-pkg))
        tooling-ack (when (and tooling-pkg (not= :refused (:phase tooling-pkg)))
                      (trecv/receive-from-r1-package tooling-pkg))
        compute-plan (when (and compute-pkg (not= :refused (:phase compute-pkg)))
                       (cmpprod/plan-from-r1 compute-pkg))
        compute-ack (when (and compute-pkg (not= :refused (:phase compute-pkg)))
                      (mrecv/receive-from-r1-package compute-pkg))
        liquidity-ack (when (and liquidity-pkg (not= :refused (:phase liquidity-pkg)))
                        (wrecv/receive-from-r1-package liquidity-pkg))
        ;; R2 execute membrane (default refuse; executed=false) — all legs present offline
        food-r2 (when food-plan (r2/refuse-without-gate "produce" food-plan))
        energy-r2 (when energy-plan (r2/refuse-without-gate "generate" energy-plan))
        care-r2 (when care-plan (r2/refuse-without-gate "produce" care-plan))
        housing-r2 (when housing-plan (r2/refuse-without-gate "grant" housing-plan))
        tooling-r2 (when tooling-plan (r2/refuse-without-gate "produce" tooling-plan))
        compute-r2 (when compute-plan (r2/refuse-without-gate "quota" compute-plan))
        liquidity-r2 (when liquidity-pkg
                       ;; liquidity residual: R2 loan refuse without dry produce plan map
                       (r2/refuse-without-gate
                        "loan"
                        (assoc liquidity-pkg
                               :cash-usd-micros 0
                               :published false
                               :server-held-key false)))
        gated-statuses [food-gated energy-gated care-gated housing-gated
                        tooling-gated compute-gated liquidity-gated]
        r2-statuses [food-r2 energy-r2 care-r2 housing-r2 tooling-r2 compute-r2 liquidity-r2]
        gated-n (count (remove nil? gated-statuses))
        gated-admissible-n (count (filter #(true? (:admissible %)) gated-statuses))
        r2-n (count (remove nil? r2-statuses))
        r2-executed-n (count (filter #(true? (:executed %)) r2-statuses))
        rail-gated
        (cond-> {}
          food-gated (assoc :food {:r1 (when food-pkg (name (:phase food-pkg)))
                                   :gated-admissible (boolean (:admissible food-gated))
                                   :executed false})
          energy-gated (assoc :energy {:r1 (when energy-pkg (name (:phase energy-pkg)))
                                       :gated-admissible (boolean (:admissible energy-gated))
                                       :executed false})
          care-gated (assoc :care {:r1 (when care-pkg (name (:phase care-pkg)))
                                   :gated-admissible (boolean (:admissible care-gated))
                                   :executed false})
          housing-gated (assoc :housing {:r1 (when housing-pkg (name (:phase housing-pkg)))
                                         :gated-admissible (boolean (:admissible housing-gated))
                                         :land-grant-executed false
                                         :executed false})
          tooling-gated (assoc :tooling {:r1 (when tooling-pkg (name (:phase tooling-pkg)))
                                         :gated-admissible (boolean (:admissible tooling-gated))
                                         :executed false})
          compute-gated (assoc :compute {:r1 (when compute-pkg (name (:phase compute-pkg)))
                                         :gated-admissible (boolean (:admissible compute-gated))
                                         :executed false})
          liquidity-gated (assoc :liquidity {:r1 (when liquidity-pkg (name (:phase liquidity-pkg)))
                                             :gated-admissible (boolean (:admissible liquidity-gated))
                                             :loan-executed false
                                             :member-principal true
                                             :cash-usd-micros 0
                                             :executed false}))
        summary {:l0-stage (or (get-in enrolled [:public-person :stage])
                               (get-in enrolled [:entitlement :stage])
                               (get-in enrolled [:vow :stage])
                               "L0")
                 :l0-published (boolean (or (get-in enrolled [:vow :published])
                                            (get-in enrolled [:entitlement :published])))
                 :l0-token-stub (or (get-in enrolled [:vow :token-id])
                                   (get-in enrolled [:entitlement :token-id]))
                 ;; liberation ladder (care/housing first at L4 = 孫/子 priority)
                 :ladder-phase (when-let [p (:phase ladder-open)] (name p))
                 :ladder-from "L0"
                 :ladder-to (or (:stage person-ladder)
                                (get-in ladder-open [:person :stage])
                                "L0")
                 :ladder-target (or (:target-stage ladder-open) ladder-target-stage)
                 :ladder-steps (count ladder-hist)
                 :ladder-rails-hint (or (:rails-hint ladder-last)
                                        (get-in lad/STAGE-FACTS
                                                [(or (:stage person-ladder) "L0") :rails-hint])
                                        [])
                 :ladder-rails-hint-first
                 (first (or (:rails-hint ladder-last)
                            (get-in lad/STAGE-FACTS
                                    [(or (:stage person-ladder) "L0") :rails-hint])))
                 :ladder-multi-gen-fact (or (:multi-gen-fact ladder-last)
                                            (get-in lad/STAGE-FACTS
                                                    [(or (:stage person-ladder) "L0") :multi-gen]))
                 :ladder-live false
                 :ladder-published false
                 :held-stress-ladder-refused
                 (boolean (and held-stress
                               (= "refused" (:ladder-advance-phase held-stress))))
                 ;; stage_sustenance after ladder (care/housing first at L4)
                 :stage-sustenance-stage (:stage stage-pkg)
                 :stage-rails (vec (or (:rails stage-pkg) []))
                 :stage-rails-first (first (:rails stage-pkg))
                 :stage-rails-second (second (:rails stage-pkg))
                 :stage-label (:label stage-pkg)
                 :stage-multi-gen-fact (:multi-gen-fact stage-pkg)
                 :stage-floor-usd-micros-yr (or (:floor-usd-micros-yr stage-pkg) 0)
                 :stage-care-hours-floor-yr
                 (or (get-in stage-pkg [:packages "care" :floor :care-hours-floor-yr]) 0)
                 :stage-housing-months-floor-yr
                 (or (get-in stage-pkg [:packages "housing" :floor :housing-months-floor-yr]) 0)
                 :stage-land-grant-executed
                 (boolean (or (get-in stage-pkg [:packages "housing" :plan :land-grant-executed])
                              (get-in stage-pkg [:gated-summary :housing-land-grant-executed])))
                 :stage-r2-count (or (:r2-count stage-pkg) (count stage-r2-phases))
                 :stage-r2-all-refused
                 (boolean (or (:r2-all-refused stage-pkg)
                              (and (seq stage-r2-phases)
                                   (every? #(= :refused %) stage-r2-phases))))
                 :stage-gated-count (or (:gated-count stage-pkg) 0)
                 :stage-gated-admissible-count (or (:gated-admissible-count stage-pkg) 0)
                 :stage-all-gated-refused (boolean (:all-gated-refused stage-pkg))
                 :stage-care-gated-admissible
                 (boolean (get-in stage-pkg [:gated-summary :care-gated-admissible]))
                 :stage-food-gated-admissible
                 (boolean (get-in stage-pkg [:gated-summary :food-gated-admissible]))
                 :stage-mitsuho-gated-admissible
                 (boolean (get-in stage-pkg [:gated-summary :food-gated-admissible]))
                 :stage-hikari-gated-admissible
                 (boolean (get-in stage-pkg [:gated-summary :energy-gated-admissible]))
                 :stage-live false
                 :stage-cash-usd-micros 0
                 :disclosure-state (name (or (:state hold) :open))
                 :entitlements-may-flow? (disc/entitlements-may-flow? hold)
                 :continuity-action (:action cont-open)
                 :mitsuho-r1-phase (when food-pkg (name (:phase food-pkg)))
                 :mitsuho-gated-admissible (boolean (:admissible food-gated))
                 :mitsuho-produce-executed false
                 :mitsuho-gated-receive-admissible
                 (boolean (:admissible food-recv-gated))
                 :mitsuho-gated-receive-phase
                 (when food-recv-gated (name (:phase food-recv-gated)))
                 :mitsuho-produce-invoked-on-receive false
                 :hikari-r1-phase (when energy-pkg (name (:phase energy-pkg)))
                 :hikari-gated-admissible (boolean (:admissible energy-gated))
                 :hikari-generate-executed false
                 :hikari-gated-receive-admissible
                 (boolean (:admissible energy-recv-gated))
                 :hikari-gated-receive-phase
                 (when energy-recv-gated (name (:phase energy-recv-gated)))
                 :hikari-generate-invoked-on-receive false
                 :mitsuho-hikari-receive-both-refused
                 (and (some? food-recv-gated)
                      (some? energy-recv-gated)
                      (not (true? (:admissible food-recv-gated)))
                      (not (true? (:admissible energy-recv-gated))))
                 :care-r1-phase (when care-pkg (name (:phase care-pkg)))
                 :care-gated-admissible (boolean (:admissible care-gated))
                 :care-delivery-executed false
                 :housing-r1-phase (when housing-pkg (name (:phase housing-pkg)))
                 :housing-gated-admissible (boolean (:admissible housing-gated))
                 :housing-land-grant-executed false
                 :tooling-r1-phase (when tooling-pkg (name (:phase tooling-pkg)))
                 :tooling-gated-admissible (boolean (:admissible tooling-gated))
                 :compute-r1-phase (when compute-pkg (name (:phase compute-pkg)))
                 :compute-gated-admissible (boolean (:admissible compute-gated))
                 :liquidity-r1-phase (when liquidity-pkg (name (:phase liquidity-pkg)))
                 :liquidity-gated-admissible (boolean (:admissible liquidity-gated))
                 :liquidity-loan-executed false
                 :liquidity-member-principal (boolean (:member-principal liquidity-pkg))
                 :liquidity-cash-usd-micros 0
                 :rails-gated-count gated-n
                 :rails-gated-admissible-count gated-admissible-n
                 :all-rails-gated-refused (and (pos? gated-n) (zero? gated-admissible-n))
                 :r2-status-count r2-n
                 :r2-executed-count r2-executed-n
                 :all-r2-not-executed (zero? r2-executed-n)
                 :rail-gated rail-gated
                 :r2-food-phase (when food-r2 (name (:phase food-r2)))
                 :r2-food-executed (boolean (:executed food-r2))
                 :r2-energy-phase (when energy-r2 (name (:phase energy-r2)))
                 :r2-energy-executed (boolean (:executed energy-r2))
                 :held-stress-held? (boolean (:held? held-stress))
                 :held-stress-food-phase
                 (when-let [p (:food-r1-when-held held-stress)]
                   (name (:phase p)))
                 :live false
                 :cash-usd-micros 0
                 :score-surface []
                 :priority-stack PRIORITY-STACK}
        out {:path "ss-offline-inkind-rails"
             :priority-stack PRIORITY-STACK
             :live false
             :cash-usd-micros 0
             :score-surface []
             :l0 enrolled
             :ladder ladder-open
             :person-after-ladder person-ladder
             :stage-sustenance stage-pkg
             :stage-sustenance-public stage-row
             :disclosure-hold hold
             :disclosure-continuity cont-open
             :disclosure-continuity-series cont-series
             :disclosure-held-stress held-stress
             :public-person (pp/public-surface person-ladder
                                               :stage (or (:stage person-ladder) "L0"))
             :food-package food-pkg
             :food-gated-live-status food-gated
             :food-gated-receive-status food-recv-gated
             :food-produce-plan food-plan
             :food-receive food-ack
             :food-r2-execute-status food-r2
             :energy-package energy-pkg
             :energy-gated-live-status energy-gated
             :energy-gated-receive-status energy-recv-gated
             :energy-produce-plan energy-plan
             :energy-receive (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                               (hrecv/receive-from-r1-package energy-pkg))
             :energy-r2-execute-status energy-r2
             :care-package care-pkg
             :care-gated-live-status care-gated
             :care-produce-plan care-plan
             :care-receive care-ack
             :care-r2-execute-status care-r2
             :housing-package housing-pkg
             :housing-gated-live-status housing-gated
             :housing-produce-plan housing-plan
             :housing-receive housing-ack
             :housing-r2-execute-status housing-r2
             :tooling-package tooling-pkg
             :tooling-gated-live-status tooling-gated
             :tooling-produce-plan tooling-plan
             :tooling-receive tooling-ack
             :tooling-r2-execute-status tooling-r2
             :compute-package compute-pkg
             :compute-gated-live-status compute-gated
             :compute-produce-plan compute-plan
             :compute-receive compute-ack
             :compute-r2-execute-status compute-r2
             :liquidity-package liquidity-pkg
             :liquidity-gated-live-status liquidity-gated
             :liquidity-receive liquidity-ack
             :liquidity-r2-execute-status liquidity-r2
             :priority-path-summary summary}]
    (pp/assert-no-public-scores! (:public-person out))
    (pp/assert-no-public-scores! (dissoc summary :rail-gated :continuity-action
                                         :stage-rails :ladder-rails-hint))
    (pp/assert-no-public-scores! stage-row)
    (doseq [g (remove nil? gated-statuses)] (pp/assert-no-public-scores! g))
    (when food-recv-gated (pp/assert-no-public-scores! food-recv-gated))
    (when energy-recv-gated (pp/assert-no-public-scores! energy-recv-gated))
    out))

#?(:clj
   (defn run-with-itonami-seed
     "Offline path + itonami displacement public facts."
     [opts]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           itonami-seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           fuchi-seed (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
           path (run-food-path opts)
           disp (itonami/public-facts-from-itonami itonami-seed fuchi-seed)]
       (assoc path :itonami-displacement disp :live false :cash-usd-micros 0))))
