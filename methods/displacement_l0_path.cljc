(ns fuchi.methods.displacement-l0-path
  "displacement_l0_path.cljc — offline path: itonami/robotics displacement → L0 enroll.

  When a funded Public-Fund earmark exists (G2), project representative displaced
  subjects into L0 enrollment, climb offline toward L2 (vowed multi-gen sustenance:
  food + energy + care + housing; wellbecoming > 孫 > 子), attach stage-aware dry
  floors. R2 execute stays refused. Unfunded surplus → refused (no free-riding).

  Never cash. Never scores. Never live mint/dispatch. Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.stage-sustenance :as stage]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Per-subject advisory floors within cohort earmark (illustrative offline).
;; L2 multi-gen package: food + care + energy + housing.
(def DEFAULT-FOOD-MICROS-YR 2000000000)
(def DEFAULT-CARE-MICROS-YR 1000000000)
(def DEFAULT-ENERGY-MICROS-YR 800000000)
(def DEFAULT-HOUSING-MICROS-YR 6000000000)

(defn subject-did-for
  "Stable offline DID stub for a displaced worker slot in a cohort."
  [cohort-id slot]
  (str "did:web:etzhayyim.com:displaced:" cohort-id ":w" slot))

(defn plan-cohort-slots
  "How many L0 slots to open offline (capped; not a ranking score)."
  [event & {:keys [max-slots] :or {max-slots 5}}]
  (let [n (long (:displaced-count event 0))
        earmark (:earmark-usd-micros-yr (couple/earmark-from-surplus event))
        per (+ DEFAULT-FOOD-MICROS-YR DEFAULT-CARE-MICROS-YR
               DEFAULT-ENERGY-MICROS-YR DEFAULT-HOUSING-MICROS-YR)
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
  "Offline L0 enroll → climb to target stage (default L2) → stage-aware dry floors.
   R2 execute remains default refuse."
  [{:keys [subject-did cohort-id displacing-actor food-imputed-usd-micros-yr
           care-imputed-usd-micros-yr energy-imputed-usd-micros-yr
           housing-imputed-usd-micros-yr vow-text member-signature
           climb-steps target-stage]
    :or {food-imputed-usd-micros-yr DEFAULT-FOOD-MICROS-YR
         care-imputed-usd-micros-yr DEFAULT-CARE-MICROS-YR
         energy-imputed-usd-micros-yr DEFAULT-ENERGY-MICROS-YR
         housing-imputed-usd-micros-yr DEFAULT-HOUSING-MICROS-YR
         climb-steps 2
         target-stage "L2"}}]
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
                         {:kind "energy" :active? true}
                         {:kind "housing" :active? true}]
                 :floor-usd-micros-yr (+ food-imputed-usd-micros-yr
                                         care-imputed-usd-micros-yr
                                         energy-imputed-usd-micros-yr
                                         housing-imputed-usd-micros-yr)
                 :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                              :wellbecoming-attest-fact :submitted
                              :related-party-edges [] :rider-s2-self-report :none}
                 :exit-suspended? false
                 :stage "L0"
                 :cohort-id cohort-id
                 :displacement-source displacing-actor
                 :cash-usd-micros 0}
        hold (dh/initial person0)
        climb (ladder/climb-offline person0 hold :steps climb-steps :member-signature sig)
        person (:person climb)
        stage (or (:stage person) "L0")
        stage-pkg (stage/build-for-stage
                   person hold
                   :imputed-overrides {"food" food-imputed-usd-micros-yr
                                       "care" care-imputed-usd-micros-yr
                                       "energy" energy-imputed-usd-micros-yr
                                       "housing" housing-imputed-usd-micros-yr})
        pkgs (:packages stage-pkg)
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
             :target-stage target-stage
             :public-person (pp/public-surface person :stage stage)
             :stage-sustenance stage-pkg
             :stage-public (stage/public-floor-row stage-pkg)
             ;; convenience aliases for tests / path consumers
             :food-package (get-in pkgs ["food" :package])
             :food-produce-plan (get-in pkgs ["food" :plan])
             :care-package (get-in pkgs ["care" :package])
             :care-produce-plan (get-in pkgs ["care" :plan])
             :energy-package (get-in pkgs ["energy" :package])
             :energy-produce-plan (get-in pkgs ["energy" :plan])
             :housing-package (get-in pkgs ["housing" :package])
             :housing-produce-plan (get-in pkgs ["housing" :plan])
             :r2-execute-status (or (get-in pkgs ["food" :r2])
                                    (get-in pkgs ["care" :r2]))}]
    (pp/assert-no-public-scores! (:public-person out))
    out))

(defn run-for-event
  "One funded displacement event → slot plan + offline L0→L2 paths (or refuse package)."
  [event & {:keys [max-slots climb-steps] :or {max-slots 5 climb-steps 2}}]
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
                 :displacing-actor (:displacing-actor event)
                 :climb-steps climb-steps}))
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
         :note "offline L0→L2 stage sustenance only — no live mint, no cash, no produce execute"}))))

(defn run-from-itonami-seed
  "All itonami seed events → displacement-L0 packages (admissible + refused)."
  [itonami-seed & {:keys [max-slots climb-steps] :or {max-slots 5 climb-steps 2}}]
  (let [events (if (sequential? itonami-seed)
                 (mapv itonami/itonami->couple-event itonami-seed)
                 (itonami/load-itonami-batch itonami-seed))
        packages (mapv #(run-for-event % :max-slots max-slots :climb-steps climb-steps) events)
        out {:path "displacement-l0-batch"
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :packages packages
             :enrolled-subjects (count (mapcat :subjects packages))
             :refused-cohorts (count (filter #(= :refused (:phase %)) packages))
             :admissible-cohorts (count (filter #(= :offline-enrolled (:phase %)) packages))
             :stage-counts (frequencies (map :stage (mapcat :subjects packages)))}]
    (doseq [p packages
            s (:subjects p)]
      (pp/assert-no-public-scores! (:public-person s)))
    out))

#?(:clj
   (defn run-default-seed
     "Load data/itonami-displacement-events.edn and run displacement→L2 offline path."
     [& {:keys [max-slots climb-steps] :or {max-slots 3 climb-steps 2}}]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))]
       (run-from-itonami-seed seed :max-slots max-slots :climb-steps climb-steps))))
