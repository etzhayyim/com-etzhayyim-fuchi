(ns fuchi.methods.ss-offline-path
  "ss_offline_path.cljc — end-to-end OFFLINE path for covenantal SS fragment.

  Priority stack path (offline only):
    (1) L0 enroll scaffold
    (2) disclosure hold machine + continuity tick (+ optional held stress)
    (3) mitsuho/hikari (and other rails) R1 → gated-live DESIGN status (default refuse)
    + dry receive / dry produce plans + R2 execute refuse
    + optional itonami displacement facts

  live=false throughout. cash≡0. liquidity is member-principal only. no scores.
  Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
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
           include-disclosure-stress]
    :or {food-imputed-usd-micros-yr 2000000000 energy-imputed-usd-micros-yr 0
         care-imputed-usd-micros-yr 0 housing-imputed-usd-micros-yr 0
         tooling-imputed-usd-micros-yr 0 compute-imputed-usd-micros-yr 0
         liquidity-imputed-usd-micros-yr 0
         include-disclosure-stress true}}]
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
        cont-series (when include-disclosure-stress
                      (disc/tick-series person [FRESH-DISC STALE-DISC FRESH-DISC]))
        held-stress (when include-disclosure-stress
                      (let [t (disc/tick hold person :disclosure STALE-DISC
                                         :reason "stale-hold-stress")]
                        {:action (:action t)
                         :to-state (:to-state t)
                         :held? (:held? t)
                         :entitlements-may-flow?
                         (disc/entitlements-may-flow? (:machine t))
                         :food-r1-when-held
                         (when (pos? food-imputed-usd-micros-yr)
                           (food/r1-dry-package
                            {:alloc-id (str "food-held-" subject-did)
                             :subject-did subject-did
                             :imputed-usd-micros-yr food-imputed-usd-micros-yr
                             :person (assoc person :disclosure STALE-DISC)
                             :hold-machine (:machine t)}))
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
        ;; priority (3): R1 → gated-live DESIGN status (default env refuse)
        food-gated (when (and food-pkg (not= :refused (:phase food-pkg)))
                     (food/gated-live-status food-pkg :hold-machine hold))
        energy-gated (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                       (energy/gated-live-status energy-pkg :hold-machine hold))
        food-plan (when (and food-pkg (not= :refused (:phase food-pkg)))
                    (mprod/plan-from-r1 food-pkg))
        energy-plan (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                      (hprod/plan-from-r1 energy-pkg))
        food-ack (when (and food-pkg (not= :refused (:phase food-pkg)))
                   (frecv/receive-from-r1-package food-pkg))
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
        ;; R2 execute membrane (default refuse; executed=false)
        food-r2 (when food-plan (r2/refuse-without-gate "produce" food-plan))
        energy-r2 (when energy-plan (r2/refuse-without-gate "generate" energy-plan))
        summary {:l0-stage (or (get-in enrolled [:public-person :stage])
                               (get-in enrolled [:entitlement :stage])
                               (get-in enrolled [:vow :stage])
                               "L0")
                 :l0-published (boolean (or (get-in enrolled [:vow :published])
                                            (get-in enrolled [:entitlement :published])))
                 :l0-token-stub (or (get-in enrolled [:vow :token-id])
                                   (get-in enrolled [:entitlement :token-id]))
                 :disclosure-state (name (or (:state hold) :open))
                 :entitlements-may-flow? (disc/entitlements-may-flow? hold)
                 :continuity-action (:action cont-open)
                 :mitsuho-r1-phase (when food-pkg (name (:phase food-pkg)))
                 :mitsuho-gated-admissible (boolean (:admissible food-gated))
                 :mitsuho-produce-executed false
                 :hikari-r1-phase (when energy-pkg (name (:phase energy-pkg)))
                 :hikari-gated-admissible (boolean (:admissible energy-gated))
                 :hikari-generate-executed false
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
             :disclosure-hold hold
             :disclosure-continuity cont-open
             :disclosure-continuity-series cont-series
             :disclosure-held-stress held-stress
             :public-person (pp/public-surface person-open :stage "L0")
             :food-package food-pkg
             :food-gated-live-status food-gated
             :food-produce-plan food-plan
             :food-receive food-ack
             :food-r2-execute-status food-r2
             :energy-package energy-pkg
             :energy-gated-live-status energy-gated
             :energy-produce-plan energy-plan
             :energy-receive (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                               (hrecv/receive-from-r1-package energy-pkg))
             :energy-r2-execute-status energy-r2
             :care-package care-pkg
             :care-produce-plan care-plan
             :care-receive care-ack
             :housing-package housing-pkg
             :housing-produce-plan housing-plan
             :housing-receive housing-ack
             :tooling-package tooling-pkg
             :tooling-produce-plan tooling-plan
             :tooling-receive tooling-ack
             :compute-package compute-pkg
             :compute-produce-plan compute-plan
             :compute-receive compute-ack
             :liquidity-package liquidity-pkg
             :liquidity-receive liquidity-ack
             :priority-path-summary summary}]
    (pp/assert-no-public-scores! (:public-person out))
    (pp/assert-no-public-scores! summary)
    (when food-gated (pp/assert-no-public-scores! food-gated))
    (when energy-gated (pp/assert-no-public-scores! energy-gated))
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
