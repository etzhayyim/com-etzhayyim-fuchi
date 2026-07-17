(ns fuchi.methods.displacement-l0-path
  "displacement_l0_path.cljc — offline path: itonami/robotics displacement → L0 enroll.

  When a funded Public-Fund earmark exists (G2), project representative displaced
  subjects into L0 enrollment + multi-gen floor rails (food + care + energy;
  wellbecoming > 孫 > 子), then offline L0→L1 ladder climb when disclosure open.
  R2 execute stays refused. Unfunded surplus → refused (no free-riding).

  Never cash. Never scores. Never live mint/dispatch. Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.rail-mitsuho :as food]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-hikari :as energy]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.r2-execute :as r2]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Per-subject advisory floors within cohort earmark (illustrative offline).
;; Multi-gen first: food + care + energy substrate (wellbecoming > 孫 > 子).
(def DEFAULT-FOOD-MICROS-YR 2000000000)   ; $2k food
(def DEFAULT-CARE-MICROS-YR 1000000000)   ; $1k care (子・孫)
(def DEFAULT-ENERGY-MICROS-YR 800000000)  ; $0.8k energy

(defn subject-did-for
  "Stable offline DID stub for a displaced worker slot in a cohort."
  [cohort-id slot]
  (str "did:web:etzhayyim.com:displaced:" cohort-id ":w" slot))

(defn- admissible-events
  "itonami/couple events that pass G2 with zero committed floor probe."
  [events]
  (filterv
   (fn [ev]
     (let [ear (couple/earmark-from-surplus ev)
           gate (couple/coupling-gate ev ear 0)]
       (true? (get gate "admissible"))))
   events))

(defn plan-cohort-slots
  "How many L0 slots to open offline (capped; not a ranking score)."
  [event & {:keys [max-slots] :or {max-slots 5}}]
  (let [n (long (:displaced-count event 0))
        earmark (:earmark-usd-micros-yr (couple/earmark-from-surplus event))
        per (+ DEFAULT-FOOD-MICROS-YR DEFAULT-CARE-MICROS-YR DEFAULT-ENERGY-MICROS-YR)
        by-budget (if (pos? per) (quot earmark per) 0)
        slots (min max-slots n (max 0 by-budget))]
    {:cohort-id (:cohort-id event)
     :displacing-actor (:displacing-actor event)
     :displaced-count n
     :earmark-usd-micros-yr earmark
     :slots slots
     :per-subject-floor-usd-micros-yr per
     :cash-usd-micros 0
     :live false
     :score-surface []}))

(defn enroll-displaced-subject
  "Offline L0 + food/care/energy R1 dry packages + produce plans + optional L0→L1 climb.
   R2 execute remains default refuse."
  [{:keys [subject-did cohort-id displacing-actor food-imputed-usd-micros-yr
           care-imputed-usd-micros-yr energy-imputed-usd-micros-yr
           vow-text member-signature climb-to-l1?]
    :or {food-imputed-usd-micros-yr DEFAULT-FOOD-MICROS-YR
         care-imputed-usd-micros-yr DEFAULT-CARE-MICROS-YR
         energy-imputed-usd-micros-yr DEFAULT-ENERGY-MICROS-YR
         climb-to-l1? true}}]
  (let [sig (or member-signature (str "sig-displaced-" subject-did))
        enrolled (l0/enroll {:subject-did subject-did
                             :vow-text (or vow-text
                                           (str "L0 after displacement cohort " cohort-id
                                                " by " displacing-actor
                                                " — multi-gen wellbecoming"))
                             :member-signature sig
                             :covenant "outreach"})
        person0 {:did subject-did
                 :covenant "vowed"
                 :rails [{:kind "food" :active? true}
                         {:kind "care" :active? true}
                         {:kind "energy" :active? true}]
                 :floor-usd-micros-yr (+ food-imputed-usd-micros-yr
                                         care-imputed-usd-micros-yr
                                         energy-imputed-usd-micros-yr)
                 :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                              :wellbecoming-attest-fact :submitted
                              :related-party-edges [] :rider-s2-self-report :none}
                 :exit-suspended? false
                 :stage "L0"
                 :cohort-id cohort-id
                 :displacement-source displacing-actor
                 :cash-usd-micros 0}
        hold (dh/initial person0)
        food-pkg (food/r1-dry-package
                  {:alloc-id (str "food-" subject-did)
                   :subject-did subject-did
                   :imputed-usd-micros-yr food-imputed-usd-micros-yr
                   :person person0
                   :hold-machine hold})
        care-pkg (care/r1-dry-package
                  {:alloc-id (str "care-" subject-did)
                   :subject-did subject-did
                   :imputed-usd-micros-yr care-imputed-usd-micros-yr
                   :person person0
                   :hold-machine hold})
        energy-pkg (energy/r1-dry-package
                    {:alloc-id (str "energy-" subject-did)
                     :subject-did subject-did
                     :imputed-usd-micros-yr energy-imputed-usd-micros-yr
                     :person person0
                     :hold-machine hold})
        food-plan (when (not= :refused (:phase food-pkg))
                    (mprod/plan-from-r1 food-pkg))
        care-plan (when (not= :refused (:phase care-pkg))
                    (cprod/plan-from-r1 care-pkg))
        energy-plan (when (not= :refused (:phase energy-pkg))
                      (hprod/plan-from-r1 energy-pkg))
        climb (when climb-to-l1?
                (ladder/climb-offline person0 hold :steps 1 :member-signature sig))
        person (if climb (:person climb) person0)
        stage (or (:stage person) "L0")
        r2-refuse (when food-plan
                    (r2/refuse-without-gate "produce" food-plan))
        out {:path "displacement-l0"
             :subject-did subject-did
             :cohort-id cohort-id
             :displacing-actor displacing-actor
             :priority-stack PRIORITY-STACK
             :live false
             :cash-usd-micros 0
             :score-surface []
             :l0 enrolled
             :disclosure-hold hold
             :ladder climb
             :ladder-fact (ladder/ladder-public-fact person)
             :stage stage
             :public-person (pp/public-surface person :stage stage)
             :food-package food-pkg
             :food-produce-plan food-plan
             :care-package care-pkg
             :care-produce-plan care-plan
             :energy-package energy-pkg
             :energy-produce-plan energy-plan
             :r2-execute-status r2-refuse}]
    (pp/assert-no-public-scores! (:public-person out))
    out))

(defn run-for-event
  "One funded displacement event → slot plan + offline L0 paths (or refuse package)."
  [event & {:keys [max-slots] :or {max-slots 5}}]
  (let [ear (couple/earmark-from-surplus event)
        gate (couple/coupling-gate event ear 0)]
    (if-not (true? (get gate "admissible"))
      {:path "displacement-l0"
       :cohort-id (:cohort-id event)
       :displacing-actor (:displacing-actor event)
       :phase :refused
       :refusal-reason (get gate "reason")
       :g2-gate gate
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK
       :subjects []}
      (let [slots-plan (plan-cohort-slots event :max-slots max-slots)
            subjects
            (mapv
             (fn [i]
               (enroll-displaced-subject
                {:subject-did (subject-did-for (:cohort-id event) i)
                 :cohort-id (:cohort-id event)
                 :displacing-actor (:displacing-actor event)}))
             (range (:slots slots-plan)))]
        {:path "displacement-l0"
         :cohort-id (:cohort-id event)
         :displacing-actor (:displacing-actor event)
         :phase :offline-enrolled
         :g2-gate gate
         :earmark ear
         :slots-plan slots-plan
         :subjects subjects
         :live false
         :cash-usd-micros 0
         :score-surface []
         :priority-stack PRIORITY-STACK
         :note "offline L0 only — no live mint, no cash, no produce execute"}))))

(defn run-from-itonami-seed
  "All itonami seed events → displacement-L0 packages (admissible + refused)."
  [itonami-seed & {:keys [max-slots] :or {max-slots 5}}]
  (let [events (if (sequential? itonami-seed)
                 (mapv itonami/itonami->couple-event itonami-seed)
                 (itonami/load-itonami-batch itonami-seed))
        packages (mapv #(run-for-event % :max-slots max-slots) events)
        out {:path "displacement-l0-batch"
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :packages packages
             :enrolled-subjects (count (mapcat :subjects packages))
             :refused-cohorts (count (filter #(= :refused (:phase %)) packages))
             :admissible-cohorts (count (filter #(= :offline-enrolled (:phase %)) packages))}]
    (doseq [p packages
            s (:subjects p)]
      (pp/assert-no-public-scores! (:public-person s)))
    out))

#?(:clj
   (defn run-default-seed
     "Load data/itonami-displacement-events.edn and run displacement→L0 offline path."
     [& {:keys [max-slots] :or {max-slots 3}}]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))]
       (run-from-itonami-seed seed :max-slots max-slots))))
