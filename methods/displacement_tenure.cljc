(ns fuchi.methods.displacement-tenure
  "displacement_tenure.cljc — offline L4→L5→L6 tenure climb after displacement enroll.

  After multi-gen L4 floors are booked within G2 earmark, optional tenure path
  climbs to L5 (commons) / L6 (covenantal tenure) with stage floors + re-book.
  Disclosure continuity is re-ticked on climb (priority #2): held disclosure
  freezes entitlement flow while public-person? may remain true.
  live=false; produce/write/commit still refuse. cash≡0. no scores.
  Portable .cljc."
  (:require [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.stage-sustenance :as stage]
            [fuchi.methods.displacement-book :as dbook]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.displacement-couple :as dcouple]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- person-from-subject
  "Carry identity + disclosure facts into tenure climb (no scores)."
  [subject]
  (let [pp (:public-person subject)
        disc (or (get-in subject [:public-person :disclosure])
                 (:disclosure subject)
                 (get-in subject [:disclosure-hold :disclosure])
                 {:wage-labor-band "0-10h" :state-benefits? false
                  :wellbecoming-attest-fact :submitted
                  :related-party-edges [] :rider-s2-self-report :none})]
    {:did (:subject-did subject)
     :covenant (or (:covenant pp) "vowed")
     :rails (or (:rails pp) [])
     :floor-usd-micros-yr (or (:imputed-fact pp) 0)
     :disclosure disc
     :exit-suspended? (boolean (or (:exit-suspended? pp)
                                   (= :exit-suspended
                                      (get-in subject [:disclosure-hold :state]))))
     :stage (or (:stage subject) "L4")
     :cohort-id (:cohort-id subject)
     :displacement-source (:displacing-actor subject)
     :cash-usd-micros 0}))

(defn tenure-climb-subject
  "Take an enrolled displacement subject (L4 typical) and climb offline to target (L5|L6).
   Continuity tick before climb (and after). Stale disclosure → held, no entitlement flow.
   Rebuilds stage floors + booking. Does not call commit_live / write_live."
  [subject & {:keys [target-stage member-signature disclosure]
              :or {target-stage "L6"}}]
  (let [person0 (person-from-subject subject)
        person0 (if disclosure (assoc person0 :disclosure disclosure) person0)
        hold0 (or (:disclosure-hold subject) (dh/initial person0))
        cont-pre (disc/tick hold0 person0
                            :disclosure (:disclosure person0)
                            :reason "pre-tenure-continuity")
        hold1 (:machine cont-pre)
        person1 (:person cont-pre)
        may-flow? (disc/entitlements-may-flow? hold1)
        sig (or member-signature (str "sig-tenure-" (:subject-did subject)))
        ;; Climb is ladder-offline even when held — floors stay plan facts, flow gates later.
        climb (ladder/climb-to-stage person1 hold1 target-stage :member-signature sig)
        person2 (:person climb)
        cont-post (disc/tick hold1 person2
                             :disclosure (:disclosure person2)
                             :reason "post-tenure-continuity")
        hold (:machine cont-post)
        person (:person cont-post)
        may-flow? (disc/entitlements-may-flow? hold)
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
             :disclosure-hold hold
             :disclosure-continuity-pre cont-pre
             :disclosure-continuity cont-post
             :disclosure-state (:state hold)
             :disclosure-held? (boolean (:entitlements-held? hold))
             :entitlements-may-flow? may-flow?
             :public-person (pp/public-surface person :stage stage)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note (if may-flow?
                     "offline tenure climb + disclosure open — no live mint/produce/commit"
                     "offline tenure climb + disclosure HELD — floors planned, flow frozen")}]
    (pp/assert-no-public-scores! (:public-person out))
    (pp/assert-no-public-scores!
     (select-keys out [:live :cash-usd-micros :score-surface :priority-stack
                       :disclosure-state :disclosure-held? :entitlements-may-flow?]))
    out))

(defn run-tenure-for-package
  "If cohort package is offline-enrolled, climb each subject to target-stage and
   re-evaluate G2 with new booked floors. Disclosure-held subjects still climb
   offline; G2 committed uses bookings (gov later zeros flowable if held)."
  [pkg event & {:keys [target-stage] :or {target-stage "L6"}}]
  (if-not (= :offline-enrolled (:phase pkg))
    (assoc pkg :tenure nil :tenure-skipped true)
    (let [ear (or (:earmark pkg) (couple/earmark-from-surplus event))
          tenured (mapv #(tenure-climb-subject % :target-stage target-stage)
                        (:subjects pkg))
          couple-ev (dcouple/commit-offline-plan event ear tenured)
          ok? (true? (:admissible couple-ev))
          held-n (count (filter :disclosure-held? tenured))
          open-n (count (filter :entitlements-may-flow? tenured))]
      (assoc pkg
             :tenure-subjects (if ok? tenured [])
             :tenure-subjects-dry (when-not ok? tenured)
             :tenure-couple couple-ev
             :tenure-phase (if ok? :tenure-offline :tenure-refused-over-earmark)
             :tenure-target (ladder/normalize-stage target-stage)
             :tenure-disclosure-open open-n
             :tenure-disclosure-held held-n
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
                   :tenure-disclosure-open
                   (reduce + 0 (map #(or (:tenure-disclosure-open %) 0) packages))
                   :tenure-disclosure-held
                   (reduce + 0 (map #(or (:tenure-disclosure-held %) 0) packages))
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    out))
