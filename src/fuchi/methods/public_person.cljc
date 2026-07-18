(ns fuchi.methods.public-person
  "public_person.cljc — as-of public-person derivation for covenantal SS recipients
  (ADR-2607177000 + ADR-2606052300 fuchi).

  public-person? is NOT a static title. It is derived from covenant + active
  rivalrous sustenance at time t. PUBLIC surface carries facts (identity, rails,
  imputed amounts, disclosure status). SCORE surface is empty: personal rank /
  percentile / happiness points are unrepresentable.

  Priority stack (Tier-0): wellbecoming > mago(孫) > ko(子) > present-adherent.
  Social security is substrate for multi-gen wellbecoming, not recipient ranking.

  Portable .cljc; pure fns; no I/O."
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def PRIORITY-STACK
  [:wellbecoming :mago-wellbecoming :ko-wellbecoming :present-adherent :operational])

(def PUBLIC-KEYS
  #{:did :sbt :covenant :stage :rails :imputed-fact :cohort-earmark
    :disclosure-status :hold-reason-code :multi-gen-care-facts
    :public-person? :exit-suspended? :receives-ss?})

(def SCORE-FORBIDDEN-KEYS
  #{:priority-rank :rank :score :percentile :wellbecoming-score
    :happiness-score :leaderboard-position :share-rank
    :liberation-kpi-personal :weight :share})

(def DISCLOSURE-MUST
  #{:wage-labor-band :state-benefits?
    :wellbecoming-attest-fact
    :related-party-edges :rider-s2-self-report})

(def INTERNAL-KEYS
  #{:tenure-weight :priority-rank :hold-signals :audit-sample-notes :weight :share})

(defn- ->str [v] (str (or v "")))

(defn- lstrip-colon [s]
  (let [s (str s)] (if (str/starts-with? s ":") (subs s 1) s)))

(defn- norm-kw [v]
  (-> (->str v) lstrip-colon str/lower-case
      (str/split #"/") last))

(defn covenant?
  "True when the person has an active covenant compatible with §1.16 / fuchi G4.
  :vowed is full; :outreach is pre-vow (may receive minimal floor → still public if rails active)."
  [person]
  (let [c (norm-kw (or (:covenant person) (get person ":maintainer/covenant") ""))]
    (contains? #{"vowed" "outreach"} c)))

(defn exit-suspended?
  [person]
  (boolean (or (:exit-suspended? person)
               (get person ":person/exit-suspended")
               (= (norm-kw (or (:status person) "")) "exit-suspended"))))

(defn active-ss-rails
  "Rivalrous in-kind rails currently active. Accepts seq of rail maps or keywords/strings."
  [person]
  (let [rails (or (:rails person)
                  (:active-rails person)
                  (get person ":person/rails")
                  [])]
    (vec (remove nil?
                 (map (fn [r]
                        (cond
                          (map? r)
                          (let [kind (or (:kind r) (:rail r) (get r ":rail/kind") (get r ":envelope/line"))
                                active? (if (contains? r :active?)
                                          (boolean (:active? r))
                                          (not= false (get r ":rail/active" true)))]
                            (when (and kind active?) (norm-kw kind)))
                          :else (when r (norm-kw r))))
                      rails)))))

(defn receives-covenantal-ss?
  "True if at least one active rivalrous SS rail, or explicit receives-ss flag, or floor>0 entitlement."
  [person]
  (or (boolean (:receives-ss? person))
      (boolean (get person ":person/receives-ss"))
      (seq (active-ss-rails person))
      (let [floor (or (:floor-usd-micros-yr person)
                      (get person ":alloc/floor-usd-micros-yr")
                      0)]
        (and (number? floor) (pos? floor)
             (covenant? person)
             (not (exit-suspended? person))))))

(defn public-person?
  "As-of derivation (ADR-2607177000). Score is never an input."
  [person]
  (and (covenant? person)
       (receives-covenantal-ss? person)
       (not (exit-suspended? person))))

(defn disclosure-fresh?
  "Each required disclosure key must be present and not marked stale/falsehood.
  Boolean false is a valid answer (e.g. state-benefits? false)."
  [disclosure]
  (let [d (or disclosure {})]
    (and (seq d)
         (every? (fn [k]
                   (let [v (if (contains? d k)
                             (get d k)
                             (if (contains? d (keyword (name k)))
                               (get d (keyword (name k)))
                               (get d (str k) ::missing)))]
                     (and (not= v ::missing)
                          (not= v :stale)
                          (not= v "stale")
                          (not= v :falsehood)
                          (not= v "falsehood")
                          (not (nil? v)))))
                 DISCLOSURE-MUST))))

(defn disclosure-ok?
  [person]
  (let [d (or (:disclosure person) (get person ":person/disclosure") {})]
    (if (public-person? person)
      (disclosure-fresh? d)
      true)))

(defn disclosure-gate
  "While public-person, disclosure must be ok; else hold entitlements.
  Returns {:admissible bool :action :pass|:hold|:n/a :reason string}."
  [person]
  (cond
    (not (public-person? person))
    {:admissible true :action :n/a :reason "not public-person at as-of; no disclosure duty"}
    (disclosure-ok? person)
    {:admissible true :action :pass :reason "disclosure fresh and signed"}
    :else
    {:admissible false :action :hold
     :reason "disclosure fail — hold entitlements; public-person status retained"}))

(defn strip-score-keys
  "Remove score/rank keys from a map for public projection.
  Does not strip meta keys like :score-surface (empty list is allowed)."
  [m]
  (when m
    (into (array-map)
          (remove (fn [[k _]]
                    (let [ks (norm-kw k)
                          kn (keyword ks)]
                      (or (contains? SCORE-FORBIDDEN-KEYS kn)
                          (contains? SCORE-FORBIDDEN-KEYS (keyword ks))
                          (contains? #{"priority-rank" "rank" "share-rank"
                                       "score" "percentile" "leaderboard-position"
                                       "wellbecoming-score" "happiness-score"
                                       "liberation-kpi-personal" "weight" "share"}
                                     ks))))
                  m))))

(defn public-surface
  "Project a person+allocation context onto the PUBLIC surface only.
  Never includes priority-rank, share, weight, or happiness scores."
  [person & {:keys [allocation stage cohort-earmark hold-reason disclosure-status]}]
  (let [pp? (public-person? person)
        rails (active-ss-rails person)
        floor (or (:floor-usd-micros-yr allocation)
                  (:floor-usd-micros-yr person)
                  0)
        imputed (or (:imputed-fact person)
                    (:imputed-usd-micros-yr person)
                    floor)
        cov (norm-kw (or (:covenant person) (get person ":maintainer/covenant") "unknown"))
        did (or (:did person) (get person ":maintainer/did") "?")
        gate (disclosure-gate person)
        surf {:did did
              :public-person? pp?
              :covenant cov
              :stage (or stage (:stage person) (get person ":person/stage") "L0")
              :rails rails
              :imputed-fact (long imputed)
              :cohort-earmark (or cohort-earmark (:cohort-earmark person))
              :disclosure-status (or disclosure-status
                                     (:action gate)
                                     (if (disclosure-ok? person) :ok :hold))
              :hold-reason-code (when (= :hold (:action gate))
                                  (or hold-reason (:reason gate)))
              :exit-suspended? (exit-suspended? person)
              :receives-ss? (receives-covenantal-ss? person)
              :multi-gen-care-facts (or (:multi-gen-care-facts person) [])
              :priority-stack PRIORITY-STACK
              :score-surface []}]
    (strip-score-keys surf)))

(defn internal-rationing
  "INTERNAL only — tenure weight / priority-rank for allocate. Must not merge into public-surface."
  [allocation]
  (when allocation
    {:tenure-weight (:weight allocation)
     :priority-rank (:priority-rank allocation)
     :share (:share allocation)
     :surface :internal}))

(defn prefer
  "Tie-break two options by PRIORITY-STACK. Each option is a map of priority→score
  where higher is better. Returns :a, :b, or :tie.
  Scores here are decision inputs for multi-gen wellbecoming — NOT personal public ranks."
  [a-scores b-scores]
  (loop [ps PRIORITY-STACK]
    (if (empty? ps)
      :tie
      (let [p (first ps)
            av (double (or (get a-scores p) 0))
            bv (double (or (get b-scores p) 0))]
        (cond (> av bv) :a
              (< av bv) :b
              :else (recur (rest ps)))))))

(defn assert-no-public-scores!
  "Structural guard: a public-surface map must not contain score-forbidden keys."
  [surface]
  (let [keys-norm (set (map (comp keyword norm-kw) (keys surface)))
        bad (set/intersection keys-norm SCORE-FORBIDDEN-KEYS)]
    (when (seq bad)
      (throw (ex-info (str "SCORE unrepresentable on public surface: " (vec bad))
                      {:bad bad})))
    true))

(defn- lookup-any
  "First present key in rec among candidates (supports false values; unlike `some`+`or`)."
  [rec ks]
  (loop [ks ks]
    (when (seq ks)
      (let [k (first ks)]
        (cond
          (contains? rec k) (get rec k)
          (and (keyword? k) (contains? rec (str k))) (get rec (str k))
          (and (string? k) (contains? rec (keyword (subs k (if (str/starts-with? k ":") 1 0)))))
          (get rec (keyword (subs k (if (str/starts-with? k ":") 1 0))))
          :else (recur (rest ks)))))))

(defn normalize-disclosure
  "Map seed/lexicon disclosure record (string-keyed EDN or keyword map) → internal disclosure map.
  wellbecoming-attest-fact of :stale / wage band \"stale\" fail freshness."
  [rec]
  (when rec
    (let [wb (norm-kw (or (lookup-any rec [":disclosure/wellbecoming-attest-fact"
                                           :wellbecoming-attest-fact
                                           ":wellbecoming-attest-fact"])
                          "submitted"))
          wage-raw (lookup-any rec [":disclosure/wage-labor-band" :wage-labor-band ":wage-labor-band"])
          wage (str (or wage-raw ""))
          rider (norm-kw (or (lookup-any rec [":disclosure/rider-s2-self-report"
                                              :rider-s2-self-report
                                              ":rider-s2-self-report"])
                             "none"))
          edges (or (lookup-any rec [":disclosure/related-party-edges"
                                     :related-party-edges
                                     ":related-party-edges"])
                    [])
          state-b (lookup-any rec [":disclosure/state-benefits" :state-benefits
                                   :state-benefits? ":state-benefits"])]
      {:wage-labor-band (if (or (= wage "stale") (= wage ":stale") (str/blank? wage))
                          :stale
                          wage)
       :state-benefits? (boolean state-b)
       :wellbecoming-attest-fact (if (= wb "stale") :stale (keyword wb))
       :related-party-edges (vec edges)
       :rider-s2-self-report (if (= rider "falsehood") :falsehood (keyword rider))
       :multi-gen-care-facts (vec (or (lookup-any rec [":disclosure/multi-gen-care-facts"
                                                       :multi-gen-care-facts])
                                      []))
       :as-of (str (or (lookup-any rec [":disclosure/as-of" :as-of]) ""))})))

(defn disclosure-for-did
  "Lookup :disclosure/batch entry for a DID."
  [seed did]
  (let [batch (or (get seed ":disclosure/batch") (:disclosure/batch seed) [])]
    (some (fn [r]
            (when (= (or (get r ":disclosure/maintainer")
                         (get r :disclosure/maintainer)
                         (get r ":maintainerDid"))
                     did)
              r))
          batch)))

(defn persons-from-seed-row
  "Build a person map from fuchi seed maintainer + envelopes + disclosure (seed or explicit)."
  [maintainer-rec envelopes disclosure]
  (let [did (get maintainer-rec ":maintainer/did")
        cov (norm-kw (get maintainer-rec ":maintainer/covenant" ":vowed"))
        rails (mapv (fn [e]
                      {:kind (norm-kw (get e ":envelope/line"))
                       :active? true
                       :imputed-usd-micros-yr (long (get e ":envelope/imputed-usd-micros-yr" 0))})
                    envelopes)
        floor (reduce + 0 (map :imputed-usd-micros-yr rails))
        d (normalize-disclosure disclosure)]
    {:did did
     :covenant cov
     :rails rails
     :floor-usd-micros-yr floor
     :disclosure (or d {})
     :multi-gen-care-facts (or (:multi-gen-care-facts d) [])
     :exit-suspended? false}))

(defn persons-from-seed
  "All public-person projections for a seed graph (ADR-2607177000)."
  [seed]
  (let [records (get seed ":maintainer/batch" [])
        env-of (fn [did]
                 (filterv #(= (get % ":envelope/maintainer") did)
                          (get seed ":envelope/batch" [])))]
    (mapv (fn [rec]
            (let [did (get rec ":maintainer/did")
                  drec (disclosure-for-did seed did)
                  person (persons-from-seed-row rec (env-of did) drec)
                  gate (disclosure-gate person)
                  surf (public-surface person
                                       :stage "L2"
                                       :disclosure-status (:action gate)
                                       :hold-reason (when (= :hold (:action gate))
                                                      (:reason gate)))]
              (assert-no-public-scores! surf)
              (assoc surf
                     :disclosure-gate gate
                     :priority-stack PRIORITY-STACK)))
          records)))
