(ns fuchi.methods.displacement-scorecard
  "displacement_scorecard.cljc — offline end-to-end scorecard for robotics/itonami SS path.

  Runs itonami seed → L0→L4 enroll/book/G2 → optional L4→L6 tenure → live-gate refuse matrix.
  Emits facts-only MD/EDN. cash≡0. no scores. live=false throughout.
  Portable .cljc; file I/O at #?(:clj) edge."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-gov :as dgov]
            [fuchi.methods.itonami-bridge :as itonami]
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

#?(:clj
   (defn- load-events []
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))]
       (itonami/load-itonami-batch
        (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))))))

(defn- ensure-tenure-and-gov
  "Attach L6 tenure (if missing) then G7 package. Public/report paths often pass bare L0 batch."
  [batch]
  (let [needs-tenure? (and (seq (:packages batch))
                           (not (some :tenure-phase (:packages batch))))
        with-ten #?(:clj
                    (if-not needs-tenure?
                      batch
                      (try
                        (let [events (load-events)
                              target (or (:tenure-target batch) "L6")]
                          (if (seq events)
                            (ten/run-batch-with-tenure batch events :target-stage target)
                            batch))
                        (catch Exception _ batch)))
                    :cljs batch)
        with-gov (if (and (seq (:packages with-ten)) (not (:gov-packaged? with-ten)))
                   (dgov/package-batch with-ten)
                   with-ten)]
    with-gov))

(defn build
  "Full offline scorecard. Default: L4 enroll + L6 tenure + G7 gov package on admissible cohorts.
   Bare L0 batches (e.g. from public surface) get tenure + gov attached automatically."
  ([]
   #?(:clj
      (let [batch (dl0/run-default-seed :max-slots 2 :climb-steps 4)]
        (build batch))
      :cljs {:error "clj-only seed load"}))
  ([batch]
   (let [batch (ensure-tenure-and-gov batch)
         pkgs (or (:packages batch) [])
         enrolled (filter #(= :offline-enrolled (:phase %)) pkgs)
         refused (filter #(#{:refused :refused-over-earmark} (:phase %)) pkgs)
         subjects (mapcat :subjects enrolled)
         tenure-subjects (mapcat :tenure-subjects pkgs)
         tenure-ok (filter #(= :tenure-offline (:tenure-phase %)) pkgs)
         live-legs (live-refuse-matrix)
         ledger #?(:clj
                   (try
                     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
                           itonami-seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
                           fuchi (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))]
                       (led/ledger-summary (led/build-ledger itonami-seed fuchi)))
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
               :scorecard/tenure-target (or (:tenure-target batch) "L6")
               :scorecard/tenure-admissible-cohorts
               (or (:tenure-admissible-cohorts batch) (count tenure-ok))
               :scorecard/tenure-subjects
               (let [n (:tenure-subjects batch)]
                 (if (number? n) n (count tenure-subjects)))
               :scorecard/tenure-stage-counts
               (frequencies (map :stage tenure-subjects))
               :scorecard/committed-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple :committed-usd-micros-yr]) 0) enrolled))
               :scorecard/headroom-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple :headroom-usd-micros-yr]) 0) enrolled))
               :scorecard/committed-post-ratify-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple-post-ratify :committed-usd-micros-yr])
                                     (get-in % [:couple :committed-post-ratify-usd-micros-yr])
                                     0)
                                enrolled))
               :scorecard/tenure-committed-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:tenure-couple :committed-usd-micros-yr]) 0)
                                tenure-ok))
               :scorecard/tenure-committed-post-ratify-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:tenure-couple-post-ratify :committed-usd-micros-yr])
                                     0)
                                tenure-ok))
               :scorecard/booked-entries
               (reduce + 0 (map #(or (get-in % [:booking :entry-count]) 0) subjects))
               :scorecard/tenure-booked-entries
               (reduce + 0 (map #(or (get-in % [:booking :entry-count]) 0) tenure-subjects))
               :scorecard/live-legs live-legs
               :scorecard/all-live-refused (every? #(false? (:admissible %)) live-legs)
               :scorecard/gov-route-counts (or (:gov-route-counts batch) {})
               :scorecard/gov-flowable-committed-usd-micros
               (or (:gov-flowable-committed-usd-micros batch) 0)
               :scorecard/gov-post-ratify-committed-usd-micros
               (or (:gov-post-ratify-committed-usd-micros batch) 0)
               :scorecard/tenure-gov-route-counts (or (:tenure-gov-route-counts batch) {})
               :scorecard/tenure-gov-flowable-committed-usd-micros
               (or (:tenure-gov-flowable-committed-usd-micros batch) 0)
               :scorecard/tenure-gov-post-ratify-committed-usd-micros
               (or (:tenure-gov-post-ratify-committed-usd-micros batch) 0)
               :scorecard/l4-disclosure-open
               (count (filter #(or (true? (:entitlements-may-flow? %))
                                   (and (nil? (:entitlements-may-flow? %))
                                        (not (true? (:disclosure-held? %)))
                                        (not= :held (get-in % [:disclosure-hold :state]))
                                        (not= :exit-suspended (get-in % [:disclosure-hold :state]))))
                              subjects))
               :scorecard/l4-disclosure-held
               (count (filter #(or (true? (:disclosure-held? %))
                                   (= :held (get-in % [:disclosure-hold :state]))
                                   (false? (:entitlements-may-flow? %)))
                              subjects))
               :scorecard/tenure-disclosure-open
               (or (:tenure-disclosure-open batch)
                   (count (filter :entitlements-may-flow? tenure-subjects)))
               :scorecard/tenure-disclosure-held
               (or (:tenure-disclosure-held batch)
                   (count (filter :disclosure-held? tenure-subjects)))
               ;; Priority #3 substrate rails: mitsuho food + hikari energy R1→gated-live
               :scorecard/mitsuho-r1-dry
               (count (filter #(= :R1-dry (get-in % [:food-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/mitsuho-gated-refused
               (count (filter #(and (:food-gated-live-status %)
                                    (false? (get-in % [:food-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/mitsuho-produce-executed
               (count (filter #(true? (get-in % [:food-produce-plan :produce-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/hikari-r1-dry
               (count (filter #(= :R1-dry (get-in % [:energy-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/hikari-gated-refused
               (count (filter #(and (:energy-gated-live-status %)
                                    (false? (get-in % [:energy-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/hikari-generate-executed
               (count (filter #(true? (get-in % [:energy-produce-plan :generate-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/care-r1-dry
               (count (filter #(= :R1-dry (get-in % [:care-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/care-gated-refused
               (count (filter #(and (:care-gated-live-status %)
                                    (false? (get-in % [:care-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/care-delivery-executed
               (count (filter #(true? (get-in % [:care-produce-plan :care-delivery-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/itonami-ledger ledger
               :scorecard/cohorts
               (mapv (fn [p]
                       {:cohort-id (:cohort-id p)
                        :displacing-actor (:displacing-actor p)
                        :phase (name (:phase p))
                        :subjects (count (:subjects p))
                        :committed (or (get-in p [:couple :committed-usd-micros-yr]) 0)
                        :committed-post-ratify
                        (or (get-in p [:couple-post-ratify :committed-usd-micros-yr]) 0)
                        :headroom (or (get-in p [:couple :headroom-usd-micros-yr]) 0)
                        :g2 (boolean (get-in p [:couple :admissible]))
                        :gov-routes (or (:gov-route-counts p) {})
                        :gov-flowable (or (:gov-flowable-committed-usd-micros p) 0)
                        :gov-post-ratify (or (:gov-post-ratify-committed-usd-micros p) 0)
                        :tenure-phase (when (:tenure-phase p) (name (:tenure-phase p)))
                        :tenure-subjects (count (:tenure-subjects p))
                        :tenure-g2 (boolean (get-in p [:tenure-couple :admissible]))
                        :tenure-committed
                        (or (get-in p [:tenure-couple :committed-usd-micros-yr]) 0)
                        :tenure-gov-flowable
                        (or (:tenure-gov-flowable-committed-usd-micros p) 0)
                        :tenure-gov-post-ratify
                        (or (:tenure-gov-post-ratify-committed-usd-micros p) 0)
                        :tenure-disclosure-open (or (:tenure-disclosure-open p) 0)
                        :tenure-disclosure-held (or (:tenure-disclosure-held p) 0)
                        :cash-usd-micros 0
                        :live false
                        :score-surface []})
                     pkgs)}]
     (pp/assert-no-public-scores!
      (dissoc body :scorecard/itonami-ledger :scorecard/cohorts :scorecard/live-legs
              :scorecard/tenure-stage-counts :scorecard/stage-counts
              :scorecard/gov-route-counts :scorecard/tenure-gov-route-counts))
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
                (str "- enrolled subjects (L4 path): " (:scorecard/enrolled-subjects body) "\n")
                (str "- stages (L4 path): " (pr-str (:scorecard/stage-counts body)) "\n")
                (str "- tenure target: " (:scorecard/tenure-target body) "\n")
                (str "- tenure admissible cohorts: " (:scorecard/tenure-admissible-cohorts body) "\n")
                (str "- tenure subjects: " (:scorecard/tenure-subjects body) "\n")
                (str "- tenure stages: " (pr-str (:scorecard/tenure-stage-counts body)) "\n")
                (str "- committed USD micros (L4): " (:scorecard/committed-usd-micros-yr body) "\n")
                (str "- headroom USD micros (L4): " (:scorecard/headroom-usd-micros-yr body) "\n")
                (str "- tenure committed USD micros (flowable-first): "
                     (:scorecard/tenure-committed-usd-micros-yr body) "\n")
                (str "- tenure post-ratify committed: "
                     (or (:scorecard/tenure-committed-post-ratify-usd-micros-yr body) 0) "\n")
                (str "- booked ledger entries (L4): " (:scorecard/booked-entries body) "\n")
                (str "- tenure booked entries: " (:scorecard/tenure-booked-entries body) "\n")
                (str "- all live legs refused: " (:scorecard/all-live-refused body) "\n")
                (str "- gov routes (L4): " (pr-str (:scorecard/gov-route-counts body)) "\n")
                (str "- gov flowable committed L4 (housing held): "
                     (:scorecard/gov-flowable-committed-usd-micros body) "\n")
                (str "- gov post-ratify committed L4 (grant false): "
                     (:scorecard/gov-post-ratify-committed-usd-micros body) "\n")
                (str "- couple post-ratify committed L4: "
                     (or (:scorecard/committed-post-ratify-usd-micros-yr body) 0) "\n")
                (str "- tenure gov routes: "
                     (pr-str (or (:scorecard/tenure-gov-route-counts body) {})) "\n")
                (str "- tenure gov flowable (housing held): "
                     (or (:scorecard/tenure-gov-flowable-committed-usd-micros body) 0) "\n")
                (str "- tenure gov post-ratify (grant false): "
                     (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros body) 0) "\n")
                (str "- L4 disclosure open/held: "
                     (or (:scorecard/l4-disclosure-open body) 0) "/"
                     (or (:scorecard/l4-disclosure-held body) 0) "\n")
                (str "- tenure disclosure open/held: "
                     (or (:scorecard/tenure-disclosure-open body) 0) "/"
                     (or (:scorecard/tenure-disclosure-held body) 0) "\n")
                (str "- mitsuho food R1-dry / gated-refused / produce-executed: "
                     (or (:scorecard/mitsuho-r1-dry body) 0) "/"
                     (or (:scorecard/mitsuho-gated-refused body) 0) "/"
                     (or (:scorecard/mitsuho-produce-executed body) 0) "\n")
                (str "- hikari energy R1-dry / gated-refused / generate-executed: "
                     (or (:scorecard/hikari-r1-dry body) 0) "/"
                     (or (:scorecard/hikari-gated-refused body) 0) "/"
                     (or (:scorecard/hikari-generate-executed body) 0) "\n")
                (str "- care-iyashi R1-dry / gated-refused / care-delivery-executed: "
                     (or (:scorecard/care-r1-dry body) 0) "/"
                     (or (:scorecard/care-gated-refused body) 0) "/"
                     (or (:scorecard/care-delivery-executed body) 0) "\n\n")
                "## Cohorts\n\n"
                "| actor | cohort | phase | n | L4-flow | L4-post | headroom | ten-flow | tenure | tenure-n |\n"
                "|---|---|---|---|---|---|---|---|---|---|\n"])]
    (doseq [c (:scorecard/cohorts body)]
      (conj! lines
             (str "| " (:displacing-actor c) " | " (:cohort-id c) " | "
                  (:phase c) " | " (:subjects c) " | " (:committed c) " | "
                  (or (:committed-post-ratify c) 0) " | "
                  (:headroom c) " | " (or (:tenure-gov-flowable c) 0) " | "
                  (or (:tenure-phase c) "—") " | "
                  (:tenure-subjects c) " |\n")))
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
         :tenure-subjects (:scorecard/tenure-subjects body)
         :priority-stack PRIORITY-STACK}))))
