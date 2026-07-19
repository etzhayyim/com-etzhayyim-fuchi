(ns fuchi.methods.disclosure-hold
  "disclosure_hold.cljc — disclosure continuity + hold state machine (ADR-2607177000).

  States:
    :open            entitlements may flow (disclosure fresh)
    :held            public-person may stay true; entitlements held
    :exit-suspended  exit — not public-person; history retained

  Events:
    :stale-detected | :falsehood | :redisclose | :lift-hold | :exit | :re-affirm

  Scores never enter this machine. cash≡0. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]))

(def STATES #{:open :held :exit-suspended})

(def EVENTS #{:stale-detected :falsehood :redisclose :lift-hold :exit :re-affirm})

(defn initial
  "Start open if disclosure fresh, else held."
  [person]
  (let [pp? (pp/public-person? person)
        ok? (pp/disclosure-ok? person)]
    {:state (cond (pp/exit-suspended? person) :exit-suspended
                  (and pp? (not ok?)) :held
                  :else :open)
     :public-person? pp?
     :entitlements-held? (and pp? (not ok?))
     :history []
     :priority-stack pp/PRIORITY-STACK
     :score-surface []}))

(defn- append-hist [m event from to reason]
  (update m :history conj {:event event :from from :to to :reason reason}))

(defn transition
  "Pure state transition. Returns new machine map or throws on illegal event."
  [machine event & {:keys [person reason disclosure]}]
  (when-not (contains? EVENTS event)
    (throw (ex-info (str "unknown event " event) {:event event})))
  (let [st (:state machine)
        reason (or reason (str event))
        person (cond-> (or person {})
                 disclosure (assoc :disclosure (pp/normalize-disclosure disclosure)
                                   ;; also accept already-normalized
                                   )
                 (and disclosure (map? disclosure) (contains? disclosure :wage-labor-band))
                 (assoc :disclosure disclosure))
        person (if (and disclosure (not (contains? (:disclosure person) :wage-labor-band)))
                 (assoc person :disclosure (or (pp/normalize-disclosure disclosure) {}))
                 person)]
    (case [st event]
      [:open :stale-detected]
      (-> machine
          (assoc :state :held :entitlements-held? true :hold-reason reason)
          (append-hist event :open :held reason))

      [:open :falsehood]
      (-> machine
          (assoc :state :held :entitlements-held? true :hold-reason reason)
          (append-hist event :open :held reason))

      [:open :exit]
      (-> machine
          (assoc :state :exit-suspended :public-person? false :entitlements-held? true
                 :hold-reason "exit")
          (append-hist event :open :exit-suspended reason))

      [:held :redisclose]
      (let [ok? (if (seq (:disclosure person))
                  (pp/disclosure-fresh? (:disclosure person))
                  false)]
        (if ok?
          (-> machine
              (assoc :state :open :entitlements-held? false :hold-reason nil
                     :public-person? (pp/public-person? (assoc person :exit-suspended? false)))
              (append-hist event :held :open "redisclose-fresh"))
          (-> machine
              (assoc :hold-reason "redisclose-still-stale")
              (append-hist event :held :held "redisclose-still-stale"))))

      [:held :lift-hold]
      ;; Council/operator lift only when disclosure ok
      (let [ok? (pp/disclosure-fresh? (or (:disclosure person) {}))]
        (if ok?
          (-> machine
              (assoc :state :open :entitlements-held? false :hold-reason nil)
              (append-hist event :held :open "lift-hold"))
          (throw (ex-info "cannot lift-hold while disclosure not fresh" {:state st}))))

      [:held :exit]
      (-> machine
          (assoc :state :exit-suspended :public-person? false :entitlements-held? true)
          (append-hist event :held :exit-suspended reason))

      [:held :stale-detected]
      (-> machine (append-hist event :held :held "already-held"))

      [:held :falsehood]
      (-> machine
          (assoc :hold-reason reason)
          (append-hist event :held :held reason))

      [:exit-suspended :re-affirm]
      (let [p (assoc person :exit-suspended? false)
            ok? (pp/disclosure-ok? p)
            pp? (pp/public-person? p)]
        (-> machine
            (assoc :state (if (and pp? (not ok?)) :held :open)
                   :public-person? pp?
                   :entitlements-held? (and pp? (not ok?))
                   :hold-reason (when (and pp? (not ok?)) "post-reaffirm-disclosure"))
            (append-hist event :exit-suspended (if (and pp? (not ok?)) :held :open) "re-affirm")))

      [:open :redisclose]
      (-> machine (append-hist event :open :open "noop-already-open"))

      [:open :lift-hold]
      (-> machine (append-hist event :open :open "noop"))

      [:open :re-affirm]
      (-> machine (append-hist event :open :open "noop"))

      [:exit-suspended :exit]
      (-> machine (append-hist event :exit-suspended :exit-suspended "already-exit"))

      (throw (ex-info (str "illegal transition " st " / " event) {:state st :event event})))))

(defn from-seed-person
  "Build machine from a public-person person map (seed-derived)."
  [person]
  (initial person))

(defn apply-disclosure-package
  "If package fresh → open (or lift); if stale → hold."
  [machine disclosure person]
  (let [d (if (and (map? disclosure) (contains? disclosure :wage-labor-band))
            disclosure
            (pp/normalize-disclosure disclosure))
        p (assoc person :disclosure d)]
    (if (pp/disclosure-fresh? d)
      (if (= :held (:state machine))
        (transition machine :redisclose :person p :disclosure d)
        (assoc machine :state :open :entitlements-held? false :hold-reason nil
               :public-person? (pp/public-person? p)))
      (if (= :open (:state machine))
        (transition machine :stale-detected :person p :reason "stale-package")
        (assoc machine :entitlements-held? true :hold-reason "stale-package")))))
