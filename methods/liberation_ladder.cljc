(ns fuchi.methods.liberation-ladder
  "liberation_ladder.cljc — offline Liberation Ladder stage progression (L0–L6).

  After L0 enroll, stages may advance offline as FACTS only when:
    - disclosure hold is :open (held/exit → refuse advance)
    - stage is adjacent next on the ladder
    - cash≡0, no server key, published=false, live=false

  Never mints SBT, never disburses, never ranks. Priority: wellbecoming > 孫 > 子.
  Portable .cljc."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Ordered Liberation Ladder (covenantal SS §1.16 style). Facts only.
(def STAGES
  ["L0"  ; outreach / enroll
   "L1"  ; community floor
   "L2"  ; vowed maintainer sustenance
   "L3"  ; deepened vocation rails
   "L4"  ; multi-gen care embedded
   "L5"  ; commons participation
   "L6"]) ; full covenantal tenure (still offline in this scaffold)

(def STAGE-SET (set STAGES))

(def STAGE-FACTS
  {"L0" {:label "outreach-enroll"
         :rails-hint ["care"]
         :multi-gen "entry-for-descendant-wellbecoming"}
   "L1" {:label "community-floor"
         :rails-hint ["care" "food"]
         :multi-gen "household-food-care-substrate"}
   "L2" {:label "vowed-sustenance"
         :rails-hint ["food" "energy" "care" "housing"]
         :multi-gen "ko-mago-housing-and-care"}
   "L3" {:label "vocation-rails"
         :rails-hint ["tooling" "compute" "food" "care"]
         :multi-gen "vocation-recovery-after-displacement"}
   "L4" {:label "multi-gen-care"
         :rails-hint ["care" "food" "housing" "energy"]
         :multi-gen "explicit-mago-ko-priority"}
   "L5" {:label "commons-participation"
         :rails-hint ["housing" "care" "compute"]
         :multi-gen "commons-land-not-private-equity"}
   "L6" {:label "covenantal-tenure"
         :rails-hint ["food" "energy" "care" "housing" "tooling" "compute"]
         :multi-gen "full-in-kind-substrate-no-cash"}})

(defn normalize-stage [s]
  (let [t (-> (str (or s "L0"))
              (str/replace #"^:" "")
              str/upper-case)]
    (if (STAGE-SET t) t "L0")))

(defn stage-index [s]
  (.indexOf STAGES (normalize-stage s)))

(defn next-stage [s]
  (let [i (stage-index s)]
    (when (and (>= i 0) (< i (dec (count STAGES))))
      (nth STAGES (inc i)))))

(defn- hold-open? [hold-machine]
  (or (nil? hold-machine)
      (and (= :open (:state hold-machine))
           (not (true? (:entitlements-held? hold-machine))))))

(defn can-advance?
  "True when offline advance is allowed (disclosure open, not at L6)."
  [person hold-machine]
  (let [st (normalize-stage (or (:stage person) "L0"))
        nxt (next-stage st)]
    (boolean
     (and nxt
          (hold-open? hold-machine)
          (not (pp/exit-suspended? person))
          (= 0 (or (:cash-usd-micros person) 0))))))

(defn refuse-advance
  [person hold-machine reason]
  (let [out {:phase :refused
             :subject-did (:did person)
             :from-stage (normalize-stage (or (:stage person) "L0"))
             :to-stage nil
             :refusal-reason reason
             :hold-state (when hold-machine (name (:state hold-machine)))
             :live false
             :cash-usd-micros 0
             :published false
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! out)
    out))

(defn advance-offline
  "Advance one stage offline. Does not mint, publish, or execute rails."
  [person hold-machine & {:keys [member-signature note]}]
  (let [from (normalize-stage (or (:stage person) "L0"))
        to (next-stage from)]
    (cond
      (pp/exit-suspended? person)
      (refuse-advance person hold-machine "exit-suspended")

      (not (hold-open? hold-machine))
      (refuse-advance person hold-machine
                      (str "disclosure/entitlements not open ("
                           (when hold-machine (name (:state hold-machine))) ")"))

      (nil? to)
      (refuse-advance person hold-machine "already-at-L6")

      :else
      (let [facts (get STAGE-FACTS to)
            out {:phase :advanced-offline
                 :subject-did (:did person)
                 :from-stage from
                 :to-stage to
                 :stage to
                 :label (:label facts)
                 :rails-hint (:rails-hint facts)
                 :multi-gen-fact (:multi-gen facts)
                 :member-signature member-signature
                 :note (or note "offline stage fact only — no live mint")
                 :live false
                 :cash-usd-micros 0
                 :published false
                 :server-held-key false
                 :score-surface []
                 :priority-stack PRIORITY-STACK}]
        (pp/assert-no-public-scores! out)
        out))))

(defn project-person
  "Return person map with updated :stage fact after successful advance."
  [person advance-result]
  (if (= :advanced-offline (:phase advance-result))
    (assoc person
           :stage (:to-stage advance-result)
           :ladder-label (:label advance-result)
           :multi-gen-care-facts (vec (distinct
                                       (concat (or (:multi-gen-care-facts person) [])
                                               [(:multi-gen-fact advance-result)]))))
    person))

(defn climb-offline
  "Advance n steps offline (default 1), stopping on first refuse."
  [person hold-machine & {:keys [steps member-signature] :or {steps 1}}]
  (loop [p person
         hm hold-machine
         n steps
         history []]
    (if (or (<= n 0) (nil? (next-stage (or (:stage p) "L0"))))
      {:person p
       :history history
       :phase (if (seq history) :climbed-offline :noop)
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK}
      (let [adv (advance-offline p hm :member-signature member-signature)]
        (if (= :refused (:phase adv))
          {:person p
           :history (conj history adv)
           :phase :stopped
           :refusal adv
           :live false
           :cash-usd-micros 0
           :score-surface []
           :priority-stack PRIORITY-STACK}
          (recur (project-person p adv) hm (dec n) (conj history adv)))))))

(defn ladder-public-fact
  "Public surface fragment for stage (no scores)."
  [person]
  (let [st (normalize-stage (or (:stage person) "L0"))
        facts (get STAGE-FACTS st)
        row {:did (:did person)
             :stage st
             :label (:label facts)
             :rails-hint (:rails-hint facts)
             :multi-gen-fact (:multi-gen facts)
             :public-person? (pp/public-person? person)
             :cash-usd-micros 0
             :live false
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! row)
    row))
