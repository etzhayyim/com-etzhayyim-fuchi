(ns fuchi.methods.displacement-tenure
  "displacement_tenure.cljc — offline L4→L5→L6 tenure climb after displacement enroll.

  After multi-gen L4 floors are booked within G2 earmark, optional tenure path
  climbs to L5 (commons) / L6 (covenantal tenure) with stage floors + re-book.
  live=false; produce/write/commit still refuse. cash≡0. no scores.
  Portable .cljc."
  (:require [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.stage-sustenance :as stage]
            [fuchi.methods.displacement-book :as dbook]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.displacement-couple :as dcouple]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn tenure-climb-subject
  "Take an enrolled displacement subject (L4 typical) and climb offline to target (L5|L6).
   Rebuilds stage floors + booking. Does not re-open G2 at subject level."
  [subject & {:keys [target-stage member-signature]
              :or {target-stage "L6"}}]
  (let [person0 {:did (:subject-did subject)
                 :covenant "vowed"
                 :rails []
                 :floor-usd-micros-yr 0
                 :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                              :wellbecoming-attest-fact :submitted
                              :related-party-edges [] :rider-s2-self-report :none}
                 :exit-suspended? false
                 :stage (or (:stage subject) "L4")
                 :cohort-id (:cohort-id subject)
                 :displacement-source (:displacing-actor subject)
                 :cash-usd-micros 0}
        hold (or (:disclosure-hold subject) (dh/initial person0))
        sig (or member-signature (str "sig-tenure-" (:subject-did subject)))
        climb (ladder/climb-to-stage person0 hold target-stage :member-signature sig)
        person (:person climb)
        stage (or (:stage person) (:stage subject))
        stage-pkg (stage/build-for-stage person hold)
        booking (dbook/book-subject (:did person) stage-pkg
                                    :alloc-id (str "tenure-" (:cohort-id subject) "-"
                                                   (:did person)))
        out {:path "displacement-tenure"
             :subject-did (:did person)
             :cohort-id (:cohort-id subject)
             :displacing-actor (:displacing-actor subject)
             :from-stage (or (:stage subject) "L4")
             :stage stage
             :target-stage (ladder/normalize-stage target-stage)
             :ladder climb
             :ladder-fact (ladder/ladder-public-fact person)
             :stage-sustenance stage-pkg
             :stage-public (stage/public-floor-row stage-pkg)
             :booking booking
             :booking-public (dbook/public-book-summary booking)
             :public-person (pp/public-surface person :stage stage)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "offline tenure climb only — no live mint/produce/commit"}]
    (pp/assert-no-public-scores! (:public-person out))
    out))

(defn run-tenure-for-package
  "If cohort package is offline-enrolled, climb each subject to target-stage and
   re-evaluate G2 with new booked floors."
  [pkg event & {:keys [target-stage] :or {target-stage "L6"}}]
  (if-not (= :offline-enrolled (:phase pkg))
    (assoc pkg :tenure nil :tenure-skipped true)
    (let [ear (or (:earmark pkg) (couple/earmark-from-surplus event))
          tenured (mapv #(tenure-climb-subject % :target-stage target-stage)
                        (:subjects pkg))
          couple-ev (dcouple/commit-offline-plan event ear tenured)
          ok? (true? (:admissible couple-ev))]
      (assoc pkg
             :tenure-subjects (if ok? tenured [])
             :tenure-subjects-dry (when-not ok? tenured)
             :tenure-couple couple-ev
             :tenure-phase (if ok? :tenure-offline :tenure-refused-over-earmark)
             :tenure-target (ladder/normalize-stage target-stage)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK))))

(defn run-batch-with-tenure
  "Run default displacement batch then optional L5/L6 tenure path per admissible cohort."
  [batch events & {:keys [target-stage] :or {target-stage "L6"}}]
  (let [ev-by (into {} (map (fn [e] [(:cohort-id e) e]) events))
        packages
        (mapv
         (fn [p]
           (if-let [ev (get ev-by (:cohort-id p))]
             (run-tenure-for-package p ev :target-stage target-stage)
             p))
         (:packages batch))
        tenure-ok (filter #(= :tenure-offline (:tenure-phase %)) packages)
        out (assoc batch
                   :packages packages
                   :tenure-target (ladder/normalize-stage target-stage)
                   :tenure-admissible-cohorts (count tenure-ok)
                   :tenure-subjects
                   (count (mapcat :tenure-subjects packages))
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    out))
