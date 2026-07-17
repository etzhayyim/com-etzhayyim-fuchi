(ns fuchi.methods.public-surface-report
  "public_surface_report.cljc — facts-only public surface for covenantal SS (ADR-2607177000).

  Emits markdown + EDN maps of public-person facts from seed. NEVER includes
  priority-rank, share, weight, scores, or percentiles.
  Portable .cljc; pure report builders; file write only at #?(:clj) edge optional."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.l0-enroll :as l0]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- last-seg [s]
  (last (str/split (str s) #":")))

(defn person-fact-row
  "One public fact row (map). Strips scores."
  [surf]
  (let [row {:did (:did surf)
             :public-person? (boolean (:public-person? surf))
             :covenant (:covenant surf)
             :stage (or (:stage surf) "L2")
             :rails (vec (or (:rails surf) []))
             :imputed-fact (or (:imputed-fact surf) 0)
             :disclosure-status (or (:disclosure-status surf)
                                    (get-in surf [:disclosure-gate :action]))
             :hold-reason-code (:hold-reason-code surf)
             :priority-stack PRIORITY-STACK
             :score-surface []}]
    (pp/assert-no-public-scores! row)
    row))

(defn seed-public-facts
  "All public-person fact rows from a seed graph."
  [seed]
  (mapv person-fact-row (pp/persons-from-seed seed)))

(defn food-energy-packages
  "R1 dry packages for food+energy for each public-person with positive floor (facts)."
  [seed]
  (let [surfs (pp/persons-from-seed seed)
        recs (get seed ":maintainer/batch" [])
        env-of (fn [did]
                 (filterv #(= (get % ":envelope/maintainer") did)
                          (get seed ":envelope/batch" [])))]
    (vec
     (for [surf surfs
           :let [did (:did surf)
                 rec (first (filter #(= (get % ":maintainer/did") did) recs))
                 envs (env-of did)
                 line* (fn [e]
                         (-> (str (get e ":envelope/line"))
                             (str/replace #"^:" "")
                             (str/replace #"^envelope/line$" "")
                             (str/split #"/")
                             last))
                 food-imp (reduce + 0 (map #(long (get % ":envelope/imputed-usd-micros-yr" 0))
                                           (filter #(= "food" (line* %)) envs)))
                 energy-imp (reduce + 0 (map #(long (get % ":envelope/imputed-usd-micros-yr" 0))
                                             (filter #(= "energy" (line* %)) envs)))
                 drec (pp/disclosure-for-did seed did)
                 person (pp/persons-from-seed-row rec envs drec)
                 hm (dh/from-seed-person person)]
           :when (or (pos? food-imp) (pos? energy-imp))]
       {:did did
        :disclosure-state (name (:state hm))
        :food (when (pos? food-imp)
                (mitsuho/r1-dry-package
                 {:alloc-id (str "food-" (last-seg did))
                  :subject-did did
                  :imputed-usd-micros-yr food-imp
                  :person person
                  :hold-machine hm}))
        :energy (when (pos? energy-imp)
                  (hikari/r1-dry-package
                   {:alloc-id (str "energy-" (last-seg did))
                    :subject-did did
                    :imputed-usd-micros-yr energy-imp
                    :person person
                    :hold-machine hm}))}))))

(defn l0-demo-fact
  "Optional L0 enroll demo fact for a DID (offline)."
  [subject-did]
  (let [e (l0/enroll {:subject-did subject-did
                      :vow-text "demo L0 for public surface report"
                      :member-signature (str "sig-" (last-seg subject-did))
                      :covenant "outreach"})]
    {:did subject-did
     :stage "L0"
     :public-person? (get-in e [:public-person :public-person?])
     :token-stub (get-in e [:vow :token-id])
     :kotoba-cid-stub (get-in e [:vow :kotoba-cid])
     :cash-usd-micros 0
     :live false
     :score-surface []
     :priority-stack PRIORITY-STACK}))

(defn report-edn
  "Full facts-only report structure."
  [seed & {:keys [include-l0-demo]}]
  (let [facts (seed-public-facts seed)
        rails (food-energy-packages seed)
        body {:report/id "fuchi.public-surface"
              :report/adr "2607177000"
              :report/priority-stack PRIORITY-STACK
              :report/score-surface []
              :report/cash-usd-micros 0
              :report/live false
              :report/public-persons facts
              :report/rail-packages rails
              :report/l0-demo (when include-l0-demo
                                (l0-demo-fact "did:web:etzhayyim.com:member:lot"))}]
    (doseq [f facts] (pp/assert-no-public-scores! f))
    body))

(defn report-md
  "Markdown facts-only public surface (no ranks/scores)."
  [seed & {:keys [include-l0-demo]}]
  (let [body (report-edn seed :include-l0-demo include-l0-demo)
        lines (transient
               ["# fuchi — public surface (facts only)\n"
                (str "Priority: wellbecoming > mago(孫) > ko(子) > present. "
                     "Scores/ranks unrepresentable. cash≡0. live=false.\n")
                "## Public persons\n"
                "| did | covenant | public? | disclosure | rails | imputed |\n"
                "|---|---|---|---|---|---|\n"])]
    (doseq [f (:report/public-persons body)]
      (conj! lines
             (str "| " (last-seg (:did f)) " | " (:covenant f) " | "
                  (:public-person? f) " | " (:disclosure-status f) " | "
                  (str/join "," (:rails f)) " | " (:imputed-fact f) " |\n")))
    (conj! lines "\n## Rail packages (R1 dry / refused)\n")
    (conj! lines "| did | food phase | energy phase |\n|---|---|---|\n")
    (doseq [r (:report/rail-packages body)]
      (conj! lines
             (str "| " (last-seg (:did r)) " | "
                  (or (some-> r :food :phase name) "—") " | "
                  (or (some-> r :energy :phase name) "—") " |\n")))
    (when-let [l0 (:report/l0-demo body)]
      (conj! lines "\n## L0 demo (offline)\n")
      (conj! lines (str "- did: " (last-seg (:did l0)) " stage=" (:stage l0)
                        " public=" (:public-person? l0) " cash=0 live=false\n")))
    (conj! lines "\n_No personal scores, ranks, or percentiles._\n")
    (apply str (persistent! lines))))

#?(:clj
   (defn write-report!
     "Write out/public-surface.md + out/public-surface.edn from seed path."
     ([]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
            seed (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
            outd (io/file actor "out")]
        (.mkdirs outd)
        (spit (io/file outd "public-surface.md") (report-md seed :include-l0-demo true))
        (spit (io/file outd "public-surface.edn")
              (pr-str (report-edn seed :include-l0-demo true)))
        {:md (str (io/file outd "public-surface.md"))
         :edn (str (io/file outd "public-surface.edn"))}))))
