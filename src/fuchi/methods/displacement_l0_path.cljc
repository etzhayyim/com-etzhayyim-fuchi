(ns fuchi.methods.displacement-l0-path
  "displacement_l0_path.cljc — offline path: itonami/robotics displacement → L0 enroll.

  When a funded Public-Fund earmark exists (G2), project representative displaced
  subjects into L0 enrollment, climb offline toward L4 (explicit 孫/子 multi-gen
  care: care/housing first + food/energy/tooling/compute), attach stage-aware dry
  floors, disclosure continuity tick, offline toritate/kanae booking (write_live
  refused). R2 execute stays refused. Unfunded surplus → refused (no free-riding).

  Never cash. Never scores. Never live mint/dispatch. Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.stage-sustenance :as stage]
            [fuchi.methods.displacement-book :as dbook]
            [fuchi.methods.displacement-couple :as dcouple]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Per-subject advisory floors within cohort earmark (illustrative offline).
;; L3: multi-gen substrate + vocation rails for robotics displacement recovery.
(def DEFAULT-FOOD-MICROS-YR 2000000000)
(def DEFAULT-CARE-MICROS-YR 1000000000)
(def DEFAULT-ENERGY-MICROS-YR 800000000)
(def DEFAULT-HOUSING-MICROS-YR 6000000000)
(def DEFAULT-TOOLING-MICROS-YR 500000000)
(def DEFAULT-COMPUTE-MICROS-YR 400000000)

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
               DEFAULT-ENERGY-MICROS-YR DEFAULT-HOUSING-MICROS-YR
               DEFAULT-TOOLING-MICROS-YR DEFAULT-COMPUTE-MICROS-YR)
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
  "Offline L0 enroll → climb to target stage (default L4 multi-gen) → stage floors + disclosure.
   R2 execute remains default refuse."
  [{:keys [subject-did cohort-id displacing-actor food-imputed-usd-micros-yr
           care-imputed-usd-micros-yr energy-imputed-usd-micros-yr
           housing-imputed-usd-micros-yr tooling-imputed-usd-micros-yr
           compute-imputed-usd-micros-yr vow-text member-signature
           climb-steps target-stage]
    :or {food-imputed-usd-micros-yr DEFAULT-FOOD-MICROS-YR
         care-imputed-usd-micros-yr DEFAULT-CARE-MICROS-YR
         energy-imputed-usd-micros-yr DEFAULT-ENERGY-MICROS-YR
         housing-imputed-usd-micros-yr DEFAULT-HOUSING-MICROS-YR
         tooling-imputed-usd-micros-yr DEFAULT-TOOLING-MICROS-YR
         compute-imputed-usd-micros-yr DEFAULT-COMPUTE-MICROS-YR
         climb-steps 4
         target-stage "L4"}}]
  ;; climb-steps 4 → L4 multi-gen-care (wellbecoming > 孫 > 子); 3 = L3 vocation
  (let [sig (or member-signature (str "sig-displaced-" subject-did))
        enrolled (l0/enroll {:subject-did subject-did
                             :vow-text (or vow-text
                                           (str "L0 after displacement cohort " cohort-id
                                                " by " displacing-actor
                                                " — multi-gen wellbecoming + vocation"))
                             :member-signature sig
                             :covenant "outreach"})
        person0 {:did subject-did
                 :covenant "vowed"
                 :rails [{:kind "food" :active? true}
                         {:kind "care" :active? true}
                         {:kind "energy" :active? true}
                         {:kind "housing" :active? true}
                         {:kind "tooling" :active? true}
                         {:kind "compute" :active? true}]
                 :floor-usd-micros-yr (+ food-imputed-usd-micros-yr
                                         care-imputed-usd-micros-yr
                                         energy-imputed-usd-micros-yr
                                         housing-imputed-usd-micros-yr
                                         tooling-imputed-usd-micros-yr
                                         compute-imputed-usd-micros-yr)
                 :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                              :wellbecoming-attest-fact :submitted
                              :related-party-edges [] :rider-s2-self-report :none}
                 :exit-suspended? false
                 :stage "L0"
                 :cohort-id cohort-id
                 :displacement-source displacing-actor
                 :cash-usd-micros 0}
        hold0 (dh/initial person0)
        ;; continuity tick with fresh disclosure (must stay open for climb/floors)
        cont (disc/tick hold0 person0 :reason "post-enroll-continuity")
        hold (:machine cont)
        person1 (:person cont)
        climb (ladder/climb-offline person1 hold :steps climb-steps :member-signature sig)
        person (:person climb)
        stage (or (:stage person) "L0")
        stage-pkg (stage/build-for-stage
                   person hold
                   :imputed-overrides {"food" food-imputed-usd-micros-yr
                                       "care" care-imputed-usd-micros-yr
                                       "energy" energy-imputed-usd-micros-yr
                                       "housing" housing-imputed-usd-micros-yr
                                       "tooling" tooling-imputed-usd-micros-yr
                                       "compute" compute-imputed-usd-micros-yr})
        pkgs (:packages stage-pkg)
        booking (dbook/book-subject subject-did stage-pkg
                                    :alloc-id (str "disp-" cohort-id "-" subject-did))
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
             :disclosure-continuity cont
             :disclosure-state (:state hold)
             :disclosure-held? (boolean (:entitlements-held? hold))
             :entitlements-may-flow? (disc/entitlements-may-flow? hold)
             :ladder climb
             :ladder-fact (ladder/ladder-public-fact person)
             :stage stage
             :target-stage target-stage
             :public-person (pp/public-surface person :stage stage)
             :stage-sustenance stage-pkg
             :stage-public (stage/public-floor-row stage-pkg)
             :food-package (get-in pkgs ["food" :package])
             :food-produce-plan (get-in pkgs ["food" :plan])
             :care-package (get-in pkgs ["care" :package])
             :care-produce-plan (get-in pkgs ["care" :plan])
             :energy-package (get-in pkgs ["energy" :package])
             :energy-produce-plan (get-in pkgs ["energy" :plan])
             :housing-package (get-in pkgs ["housing" :package])
             :housing-produce-plan (get-in pkgs ["housing" :plan])
             :tooling-package (get-in pkgs ["tooling" :package])
             :tooling-produce-plan (get-in pkgs ["tooling" :plan])
             :compute-package (get-in pkgs ["compute" :package])
             :compute-produce-plan (get-in pkgs ["compute" :plan])
             :r2-execute-status (or (get-in pkgs ["food" :r2])
                                    (get-in pkgs ["tooling" :r2])
                                    (get-in pkgs ["care" :r2]))
             :booking booking
             :booking-public (dbook/public-book-summary booking)}
        food-st (when-let [fp (:food-package out)]
                  (mitsuho/gated-live-status fp :hold-machine hold))
        energy-st (when-let [ep (:energy-package out)]
                    (hikari/gated-live-status ep :hold-machine hold))
        care-st (when-let [cp (:care-package out)]
                  (care/gated-live-status cp :hold-machine hold))
        hous-st (when-let [hp (:housing-package out)]
                  (housing/gated-live-status hp :hold-machine hold
                                             :council-housing-held? false))
        tool-st (when-let [tp (:tooling-package out)]
                  (tooling/gated-live-status tp :hold-machine hold))
        comp-st (when-let [cp (:compute-package out)]
                  (compute/gated-live-status cp :hold-machine hold))
        out (cond-> out
              food-st (assoc :food-gated-live-status food-st)
              energy-st (assoc :energy-gated-live-status energy-st)
              care-st (assoc :care-gated-live-status care-st)
              hous-st (assoc :housing-gated-live-status hous-st
                             :land-grant-executed false)
              tool-st (assoc :tooling-gated-live-status tool-st)
              comp-st (assoc :compute-gated-live-status comp-st))]
    (pp/assert-no-public-scores! (:public-person out))
    (doseq [st [food-st energy-st care-st hous-st tool-st comp-st]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn run-for-event
  "One funded displacement event → slot plan + offline L0→L4 + book + G2 re-gate.
   If booked floors exceed earmark, phase becomes :refused-over-earmark (subjects retained
   as dry plan for diagnosis, not admissible)."
  [event & {:keys [max-slots climb-steps] :or {max-slots 5 climb-steps 4}}]
  (let [ear (couple/earmark-from-surplus event)
        gate0 (couple/coupling-gate event ear 0)]
    (if-not (true? (get gate0 "admissible"))
      {:path "displacement-l0"
       :cohort-id (:cohort-id event)
       :displacing-actor (:displacing-actor event)
       :phase :refused
       :refusal-reason (get gate0 "reason")
       :g2-gate gate0
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK
       :subjects []
       :couple nil}
      (let [slots-plan (plan-cohort-slots event :max-slots max-slots)
            subjects
            (mapv
             (fn [i]
               (enroll-displaced-subject
                {:subject-did (subject-did-for (:cohort-id event) i)
                 :cohort-id (:cohort-id event)
                 :displacing-actor (:displacing-actor event)
                 :climb-steps climb-steps}))
             (range (:slots slots-plan)))
            couple-ev (dcouple/commit-offline-plan event ear subjects)
            ok? (true? (:admissible couple-ev))]
        {:path "displacement-l0"
         :cohort-id (:cohort-id event)
         :displacing-actor (:displacing-actor event)
         :phase (if ok? :offline-enrolled :refused-over-earmark)
         :refusal-reason (when-not ok? (:reason couple-ev))
         :g2-gate gate0
         :g2-committed couple-ev
         :earmark ear
         :slots-plan slots-plan
         :subjects (if ok? subjects [])
         :subjects-dry (when-not ok? subjects)
         :couple couple-ev
         :live false
         :cash-usd-micros 0
         :score-surface []
         :priority-stack PRIORITY-STACK
         :note (if ok?
                 "offline L0→L4 multi-gen floors booked within earmark — no live mint/execute/commit"
                 "booked floors exceed earmark — G2 refuse over-commit")}))))

(defn run-from-itonami-seed
  "All itonami seed events → displacement packages (admissible + refused)."
  [itonami-seed & {:keys [max-slots climb-steps] :or {max-slots 5 climb-steps 4}}]
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
             :refused-cohorts (count (filter #(#{:refused :refused-over-earmark} (:phase %)) packages))
             :admissible-cohorts (count (filter #(= :offline-enrolled (:phase %)) packages))
             :stage-counts (frequencies (map :stage (mapcat :subjects packages)))
             :committed-usd-micros-yr
             (reduce + 0 (map #(or (get-in % [:couple :committed-usd-micros-yr]) 0)
                              (filter #(= :offline-enrolled (:phase %)) packages)))}]
    (doseq [p packages
            s (:subjects p)]
      (pp/assert-no-public-scores! (:public-person s)))
    out))

#?(:clj
   (defn run-default-seed
     "Load data/itonami-displacement-events.edn and run displacement→L4 offline path."
     [& {:keys [max-slots climb-steps] :or {max-slots 3 climb-steps 4}}]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (io/file "."))
           seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))]
       (run-from-itonami-seed seed :max-slots max-slots :climb-steps climb-steps))))
