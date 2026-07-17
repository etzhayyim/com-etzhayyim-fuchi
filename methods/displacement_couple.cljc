(ns fuchi.methods.displacement-couple
  "displacement_couple.cljc — offline G2 earmark headroom vs booked displacement floors.

  After subjects are enrolled and booked offline, re-evaluate coupling-gate with
  the REAL committed in-kind total (not zero probe). commit_live remains refused
  by default (couple leg Lv7+). cash≡0. no scores. Portable .cljc."
  (:require [fuchi.methods.couple :as couple]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn committed-from-subjects
  "Sum offline booked in-kind totals. Prefers flowable-booking (gov partial hold)
   over full booking so housing held under Council does not consume earmark headroom."
  [subjects]
  (reduce + 0
          (map (fn [s]
                 (long (or (get-in s [:flowable-booking :in-kind-total-usd-micros])
                           (get-in s [:booking :in-kind-total-usd-micros])
                           (get-in s [:stage-sustenance :floor-usd-micros-yr])
                           0)))
               subjects)))

(defn committed-full-from-subjects
  "Full stage booking total (includes housing even if gov-held)."
  [subjects]
  (reduce + 0
          (map (fn [s]
                 (long (or (get-in s [:booking :in-kind-total-usd-micros])
                           (get-in s [:stage-sustenance :floor-usd-micros-yr])
                           0)))
               subjects)))

(defn evaluate-cohort
  "G2 gate with earmark + committed floors from subjects. Offline only.
   Uses flowable-first committed (partial hold aware)."
  [event earmark subjects]
  (let [committed (committed-from-subjects subjects)
        committed-full (committed-full-from-subjects subjects)
        gate (couple/coupling-gate event earmark committed)
        gate-full (couple/coupling-gate event earmark committed-full)
        live-st (live-gate/gate-status
                 (live-gate/make-live-gate {:leg "couple"}) {})
        out {:phase (if (true? (get gate "admissible"))
                      :g2-covered
                      :g2-refused)
             :cohort-id (:cohort-id event)
             :displacing-actor (:displacing-actor event)
             :earmark-usd-micros-yr (:earmark-usd-micros-yr earmark)
             :tithe-usd-micros (:tithe-usd-micros earmark)
             :gross-usd-micros-yr (:gross-usd-micros-yr earmark)
             :funded (boolean (:funded earmark))
             :committed-usd-micros-yr committed
             :committed-full-usd-micros-yr committed-full
             :headroom-usd-micros-yr (long (get gate "headroom" 0))
             :headroom-full-usd-micros-yr (long (get gate-full "headroom" 0))
             :admissible (boolean (get gate "admissible"))
             :admissible-if-full-booked (boolean (get gate-full "admissible"))
             :reason (get gate "reason")
             :subject-count (count subjects)
             :commit-live-admissible (boolean (get live-st "admissible"))
             :commit-live-status live-st
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "offline G2 (flowable-first) — housing held under Council excluded from committed"}]
    (pp/assert-no-public-scores! out)
    out))

(defn commit-offline-plan
  "Record offline commitment PLAN when G2 covered. Does not call commit_live."
  [event earmark subjects]
  (let [ev (evaluate-cohort event earmark subjects)]
    (if-not (:admissible ev)
      (assoc ev :phase :refused :committed false)
      (assoc ev
             :phase :committed-offline-plan
             :committed true
             :commit-live false
             :note "offline earmark commitment plan only — not live couple commit"))))

(defn public-couple-summary
  "Facts-only public row for reports."
  [ev]
  (let [row (select-keys ev
                         [:cohort-id :displacing-actor :phase :funded
                          :earmark-usd-micros-yr :committed-usd-micros-yr
                          :headroom-usd-micros-yr :admissible :subject-count
                          :cash-usd-micros :live :score-surface :priority-stack])]
    (pp/assert-no-public-scores! row)
    row))
