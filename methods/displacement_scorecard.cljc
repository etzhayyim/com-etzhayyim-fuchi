(ns fuchi.methods.displacement-scorecard
  "displacement_scorecard.cljc — offline end-to-end scorecard for robotics/itonami SS path.

  Runs itonami seed → displacement L0→L3 enroll/book/G2 → live-gate refuse matrix.
  Emits facts-only MD/EDN. cash≡0. no scores. live=false throughout.
  Portable .cljc; file I/O at #?(:clj) edge."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.itonami-surplus-ledger :as led]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn live-refuse-matrix
  "All live legs default refuse status (facts only)."
  ([]
   (live-refuse-matrix {}))
  ([env]
   (mapv
    (fn [[leg _]]
      (let [st (live-gate/gate-status (live-gate/make-live-gate {:leg leg}) env)]
        {:leg leg
         :admissible (boolean (get st "admissible"))
         :reason (get st "reason")
         :live false
         :cash-usd-micros 0
         :score-surface []}))
    live-gate/LEG-POLICY)))

(defn build
  "Full offline scorecard map from optional itonami batch result."
  ([]
   #?(:clj (build (dl0/run-default-seed :max-slots 2 :climb-steps 4))
      :cljs {:error "clj-only seed load"}))
  ([batch]
   (let [pkgs (or (:packages batch) [])
         enrolled (filter #(= :offline-enrolled (:phase %)) pkgs)
         refused (filter #(#{:refused :refused-over-earmark} (:phase %)) pkgs)
         subjects (mapcat :subjects enrolled)
         live-legs (live-refuse-matrix)
         ledger #?(:clj
                   (try
                     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
                           itonami (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
                           fuchi (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))]
                       (led/ledger-summary (led/build-ledger itonami fuchi)))
                     (catch Exception _ {:events 0 :live false :cash-to-workers-usd-micros 0}))
                   :cljs {:events 0})
         body {:scorecard/id "fuchi.displacement-ss-offline"
               :scorecard/adr ["2607177000" "2606032130" "2606052300"]
               :scorecard/priority-stack PRIORITY-STACK
               :scorecard/live false
               :scorecard/cash-usd-micros 0
               :scorecard/score-surface []
               :scorecard/batch-path (:path batch)
               :scorecard/admissible-cohorts (count enrolled)
               :scorecard/refused-cohorts (count refused)
               :scorecard/enrolled-subjects (count subjects)
               :scorecard/stage-counts (frequencies (map :stage subjects))
               :scorecard/committed-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple :committed-usd-micros-yr]) 0) enrolled))
               :scorecard/headroom-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple :headroom-usd-micros-yr]) 0) enrolled))
               :scorecard/booked-entries
               (reduce + 0 (map #(or (get-in % [:booking :entry-count]) 0) subjects))
               :scorecard/live-legs live-legs
               :scorecard/all-live-refused (every? #(false? (:admissible %)) live-legs)
               :scorecard/itonami-ledger ledger
               :scorecard/cohorts
               (mapv (fn [p]
                       {:cohort-id (:cohort-id p)
                        :displacing-actor (:displacing-actor p)
                        :phase (name (:phase p))
                        :subjects (count (:subjects p))
                        :committed (or (get-in p [:couple :committed-usd-micros-yr]) 0)
                        :headroom (or (get-in p [:couple :headroom-usd-micros-yr]) 0)
                        :g2 (boolean (get-in p [:couple :admissible]))
                        :cash-usd-micros 0
                        :live false
                        :score-surface []})
                     pkgs)}]
     (pp/assert-no-public-scores! (dissoc body :scorecard/itonami-ledger :scorecard/cohorts :scorecard/live-legs))
     (doseq [c (:scorecard/cohorts body)] (pp/assert-no-public-scores! c))
     body)))

(defn scorecard-md
  "Markdown scorecard (facts only)."
  [body]
  (let [lines (transient
               ["# fuchi — displacement SS offline scorecard\n\n"
                (str "Priority: wellbecoming > mago(孫) > ko(子) > present. "
                     "cash≡0. live=false. No personal scores.\n\n")
                "## Summary\n\n"
                (str "- admissible cohorts: " (:scorecard/admissible-cohorts body) "\n")
                (str "- refused cohorts: " (:scorecard/refused-cohorts body) "\n")
                (str "- enrolled subjects: " (:scorecard/enrolled-subjects body) "\n")
                (str "- stages: " (pr-str (:scorecard/stage-counts body)) "\n")
                (str "- committed USD micros: " (:scorecard/committed-usd-micros-yr body) "\n")
                (str "- headroom USD micros: " (:scorecard/headroom-usd-micros-yr body) "\n")
                (str "- booked ledger entries: " (:scorecard/booked-entries body) "\n")
                (str "- all live legs refused: " (:scorecard/all-live-refused body) "\n\n")
                "## Cohorts\n\n"
                "| actor | cohort | phase | subjects | committed | headroom | g2 |\n"
                "|---|---|---|---|---|---|---|\n"])]
    (doseq [c (:scorecard/cohorts body)]
      (conj! lines
             (str "| " (:displacing-actor c) " | " (:cohort-id c) " | "
                  (:phase c) " | " (:subjects c) " | " (:committed c) " | "
                  (:headroom c) " | " (:g2 c) " |\n")))
    (conj! lines "\n## Live legs (default refuse)\n\n")
    (conj! lines "| leg | admissible | reason |\n|---|---|---|\n")
    (doseq [l (:scorecard/live-legs body)]
      (conj! lines
             (str "| " (:leg l) " | " (:admissible l) " | "
                  (or (:reason l) "—") " |\n")))
    (when-let [led (:scorecard/itonami-ledger body)]
      (conj! lines "\n## itonami surplus ledger (offline)\n\n")
      (conj! lines (str "- events: " (:events led) "\n"))
      (conj! lines (str "- funded-admissible: " (:funded-admissible led) "\n"))
      (conj! lines (str "- refused: " (:refused led) "\n"))
      (conj! lines (str "- cash-to-workers: " (or (:cash-to-workers-usd-micros led) 0) "\n")))
    (conj! lines "\n_No personal scores, ranks, or percentiles. No live disbursement._\n")
    (apply str (persistent! lines))))

#?(:clj
   (defn write-scorecard!
     "Write out/displacement-scorecard.{md,edn}."
     ([]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
            body (build)
            outd (io/file actor "out")]
        (.mkdirs outd)
        (spit (io/file outd "displacement-scorecard.md") (scorecard-md body))
        (spit (io/file outd "displacement-scorecard.edn") (pr-str body))
        {:md (str (io/file outd "displacement-scorecard.md"))
         :edn (str (io/file outd "displacement-scorecard.edn"))
         :live false
         :cash-usd-micros 0
         :score-surface []
         :all-live-refused (:scorecard/all-live-refused body)
         :priority-stack PRIORITY-STACK}))))
