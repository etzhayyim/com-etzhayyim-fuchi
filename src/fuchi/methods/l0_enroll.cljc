(ns fuchi.methods.l0-enroll
  "l0_enroll.cljc — offline 信者 Level 0 enrollment scaffold (§1.16.3a + ADR-2607177000).

  Path (dry-run only):
    draft-vow → triple-permanent content-address stubs → L0 entitlement projection
      → optional public-person surface (if rivalrous floor rails attached)

  NEVER live: no SBT mint, no IPFS pin, no openmail, no cash (G2/G9/G10).
  Priority stack fact embedded: wellbecoming > mago > ko > present.
  Portable .cljc; pure fns."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def L0-FLOOR-USD-MICROS-YR 300000000) ; $300/yr advisory+community (ADR-2605261000 L1 band as L0 floor proxy)

(defn- ->str [v] (str (or v "")))

(defn- fnv1a64
  "Stable non-crypto content digest for offline CIDs (not security; reproducibility only)."
  [s]
  (let [s (str s)
        prime 1099511628211
        offset -3750763034362895579]
    (loop [i 0 h offset]
      (if (>= i (count s))
        #?(:clj (Long/toUnsignedString (long h) 16)
           :cljs (.toString (js/BigInt h) 16))
        (let [c #?(:clj (int (.charAt ^String s i))
                   :cljs (.charCodeAt s i))
              h' (bit-xor h c)
              h2 (unchecked-multiply h' prime)]
          (recur (inc i) h2))))))

(defn content-cid
  "Offline content-address stub: bafy-offline-<hex>."
  [s]
  (str "bafy-offline-" (fnv1a64 (str s))))

(defn assert-no-cash! [m]
  (when (and (contains? m :cash-usd-micros) (not= 0 (:cash-usd-micros m)))
    (throw (ex-info "cash≡0 INVARIANT (G2/N1): L0 enroll never disburses cash" m)))
  (when (and (contains? m :cashUsdMicros) (not= 0 (:cashUsdMicros m)))
    (throw (ex-info "cash≡0 INVARIANT (G2/N1)" m)))
  true)

(defn assert-no-server-key! [m]
  (when (or (true? (:server-held-key m)) (true? (:serverHeldKey m)))
    (throw (ex-info "no-server-key INVARIANT (G9)" m)))
  true)

(defn draft-vow
  "Draft a permanent commitment vow (pre-triple-commit). Coercion check: subject must self-sign intent."
  [{:keys [subject-did vow-text covenant member-signature multi-gen-note]
    :or {covenant "vowed" multi-gen-note "enroll-for-descendant-wellbecoming"}}]
  (when (str/blank? (str subject-did))
    (throw (ex-info "subject-did required" {})))
  (when (str/blank? (str vow-text))
    (throw (ex-info "vow-text required (metanoia/baptism/tokudo content)" {})))
  (when (str/blank? (str member-signature))
    (throw (ex-info "member-signature required (anti-coercion / no-server-key)" {})))
  (let [c (-> covenant str (str/replace #"^:" "") str/lower-case)]
    (when-not (contains? #{"outreach" "vowed"} c)
      (throw (ex-info (str "G4: covenant " c " unrepresentable") {:covenant c})))
    {:subject-did subject-did
     :vow-text vow-text
     :covenant c
     :member-signature member-signature
     :multi-gen-note multi-gen-note
     :priority-stack PRIORITY-STACK
     :phase :drafted
     :cash-usd-micros 0
     :server-held-key false
     :published false}))

(defn triple-permanent
  "Apply kotoba + IPFS + token content-address stubs. published stays false (offline)."
  [draft]
  (assert-no-cash! draft)
  (assert-no-server-key! draft)
  (when-not (= :drafted (:phase draft))
    (throw (ex-info "triple-permanent requires :drafted phase" {:phase (:phase draft)})))
  (let [body (str (:subject-did draft) "|" (:vow-text draft) "|" (:member-signature draft))
        kotoba (content-cid (str "kotoba|" body))
        ipfs (content-cid (str "ipfs|" body))
        token (str "sbt-offline-" (content-cid (str "sbt|" (:subject-did draft))))]
    (assoc draft
           :phase :committed-offline
           :stage "L0"
           :kotoba-cid kotoba
           :ipfs-cid ipfs
           :token-id token
           :covenant "vowed" ; L0 enroll seals vowed
           :published false
           :cash-usd-micros 0
           :server-held-key false)))

(defn l0-entitlement
  "Project L0 in-kind entitlement facts (advisory + community floor). No score, cash≡0.
  Rails are dry-run projections — not live provision."
  [committed]
  (when-not (= :committed-offline (:phase committed))
    (throw (ex-info "l0-entitlement requires :committed-offline" {:phase (:phase committed)})))
  (assert-no-cash! committed)
  {:subject-did (:subject-did committed)
   :stage "L0"
   :token-id (:token-id committed)
   :covenant "vowed"
   :floor-usd-micros-yr L0-FLOOR-USD-MICROS-YR
   :rails [{:kind "care" :active? true :note "advisory-community-L0"}]
   :cash-usd-micros 0
   :published false
   :server-held-key false
   :priority-stack PRIORITY-STACK
   :score-surface []
   :phase :l0-entitled})

(defn enroll
  "Full offline L0 path: draft → triple → L0 entitlement → public-person projection."
  [opts]
  (let [draft (draft-vow opts)
        committed (triple-permanent draft)
        ent (l0-entitlement committed)
        person {:did (:subject-did ent)
                :covenant "vowed"
                :rails (:rails ent)
                :floor-usd-micros-yr (:floor-usd-micros-yr ent)
                :stage "L0"
                :exit-suspended? false
                :disclosure (or (:disclosure opts)
                                {:wage-labor-band "0-10h"
                                 :state-benefits? false
                                 :wellbecoming-attest-fact :submitted
                                 :related-party-edges []
                                 :rider-s2-self-report :none})
                :multi-gen-care-facts [(:multi-gen-note committed)]}
        gate (pp/disclosure-gate person)
        surface (pp/public-surface person
                                   :stage "L0"
                                   :disclosure-status (:action gate)
                                   :hold-reason (when (= :hold (:action gate)) (:reason gate)))]
    (pp/assert-no-public-scores! surface)
    {:vow committed
     :entitlement ent
     :public-person surface
     :disclosure-gate gate
     :live false
     :priority-stack PRIORITY-STACK}))

(defn enroll-record
  "Lexicon-shaped record for com.etzhayyim.fuchi.commitmentVow (offline)."
  [enrolled]
  (let [v (:vow enrolled)]
    {:subjectDid (:subject-did v)
     :vowText (:vow-text v)
     :covenant (:covenant v)
     :stage (:stage v)
     :kotobaCid (:kotoba-cid v)
     :ipfsCid (:ipfs-cid v)
     :tokenId (:token-id v)
     :priorityStack (mapv name PRIORITY-STACK)
     :published false
     :cashUsdMicros 0
     :serverHeldKey false
     :memberSignature (:member-signature v)
     :multiGenNote (:multi-gen-note v)}))
