(ns fuchi.methods.disclosure-continuity
  "disclosure_continuity.cljc — offline continuous disclosure re-evaluation tick.

  Priority (2) from covenantal SS offline scaffold: disclosure must stay fresh
  or entitlements hold while public-person? may remain true. No scores. cash≡0.

  tick: re-apply disclosure package → open | held | exit-suspended transitions.
  Portable .cljc."
  (:require [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn tick
  "One continuity tick. disclosure optional (defaults to person :disclosure).
   Returns {:machine :person :action :held? :live false ...}."
  [machine person & {:keys [disclosure reason]}]
  (let [d (or disclosure (:disclosure person) {})
        p (assoc person :disclosure
                 (if (and (map? d) (contains? d :wage-labor-band))
                   d
                   (or (pp/normalize-disclosure d) d)))
        before (:state machine)
        m2 (dh/apply-disclosure-package machine d p)
        after (:state m2)
        action (cond
                 (= before after) :noop
                 (and (= before :open) (= after :held)) :held-on-stale
                 (and (= before :held) (= after :open)) :reopened
                 :else :transitioned)
        out {:machine m2
             :person (assoc p
                            :exit-suspended? (= :exit-suspended after)
                            :disclosure-status (if (= :open after) :pass :hold))
             :action action
             :from-state before
             :to-state after
             :held? (true? (:entitlements-held? m2))
             :reason (or reason (:hold-reason m2))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! (dissoc out :machine :person))
    out))

(defn tick-series
  "Apply successive disclosure snapshots (continuity stress). Offline only."
  [person disclosures]
  (loop [m (dh/initial person)
         p person
         ds disclosures
         hist []]
    (if-not (seq ds)
      {:person p
       :machine m
       :history hist
       :final-state (:state m)
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK}
      (let [t (tick m p :disclosure (first ds))
            m2 (:machine t)
            p2 (:person t)]
        (recur m2 p2 (rest ds) (conj hist (dissoc t :machine :person)))))))

(defn entitlements-may-flow?
  "True only when hold is open (not held, not exit)."
  [machine]
  (and (= :open (:state machine))
       (not (true? (:entitlements-held? machine)))))
