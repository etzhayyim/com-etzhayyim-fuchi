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
            [fuchi.methods.ss-offline-path :as ss-path]
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

(defn- r2-statuses-from-packages
  "Collect R2 execute-membrane status maps from gov subjects (+ subject fallback)."
  [pkgs]
  (vec
   (mapcat
    (fn [p]
      (concat
       (mapcat (fn [g] (vals (or (:r2-by-rail g) {})))
               (concat (or (:gov-subjects p) [])
                       (or (:tenure-gov-subjects p) [])))
       (keep :r2-execute-status
             (concat (or (:subjects p) [])
                     (or (:tenure-subjects p) [])))))
    pkgs)))

(defn ss-priority-path-scorecard-fact
  "Embed ss_offline_path priority (1)(2)(3) demo into scorecard (facts only).
   Full rails gated-live DESIGN refuse + R2 refuse. cash≡0. live=false."
  []
  (try
    (let [path (ss-path/run-food-path
                {:subject-did "did:web:etzhayyim.com:member:ss-scorecard-demo"
                 :food-imputed-usd-micros-yr 2000000000
                 :energy-imputed-usd-micros-yr 1500000000
                 :care-imputed-usd-micros-yr 1000000000
                 :housing-imputed-usd-micros-yr 12000000000
                 :tooling-imputed-usd-micros-yr 500000000
                 :compute-imputed-usd-micros-yr 800000000
                 :liquidity-imputed-usd-micros-yr 1500000000
                 :include-disclosure-stress true})
          s (or (:priority-path-summary path) {})
          out {:path (:path path)
               :l0-stage (:l0-stage s)
               :l0-published (boolean (:l0-published s))
               :ladder-from (:ladder-from s)
               :ladder-to (:ladder-to s)
               :ladder-target (:ladder-target s)
               :ladder-steps (or (:ladder-steps s) 0)
               :ladder-phase (:ladder-phase s)
               :ladder-rails-hint-first (:ladder-rails-hint-first s)
               :ladder-published false
               :held-stress-ladder-refused (boolean (:held-stress-ladder-refused s))
               :stage-sustenance-stage (:stage-sustenance-stage s)
               :stage-rails-first (:stage-rails-first s)
               :stage-rails-second (:stage-rails-second s)
               :stage-floor-usd-micros-yr (or (:stage-floor-usd-micros-yr s) 0)
               :stage-care-hours-floor-yr (or (:stage-care-hours-floor-yr s) 0)
               :stage-housing-months-floor-yr (or (:stage-housing-months-floor-yr s) 0)
               :stage-land-grant-executed (boolean (:stage-land-grant-executed s))
               :stage-r2-all-refused (boolean (:stage-r2-all-refused s))
               :stage-gated-count (or (:stage-gated-count s) 0)
               :stage-all-gated-refused (boolean (:stage-all-gated-refused s))
               :stage-care-gated-admissible (boolean (:stage-care-gated-admissible s))
               :stage-mitsuho-gated-admissible (boolean (:stage-mitsuho-gated-admissible s))
               :stage-hikari-gated-admissible (boolean (:stage-hikari-gated-admissible s))
               :disclosure-state (:disclosure-state s)
               :entitlements-may-flow? (boolean (:entitlements-may-flow? s))
               :held-stress-held? (boolean (:held-stress-held? s))
               :held-stress-food-phase (:held-stress-food-phase s)
               :rails-gated-count (or (:rails-gated-count s) 0)
               :rails-gated-admissible-count (or (:rails-gated-admissible-count s) 0)
               :all-rails-gated-refused (boolean (:all-rails-gated-refused s))
               :r2-status-count (or (:r2-status-count s) 0)
               :r2-executed-count (or (:r2-executed-count s) 0)
               :all-r2-not-executed (boolean (:all-r2-not-executed s true))
               :mitsuho-gated-admissible (boolean (:mitsuho-gated-admissible s))
               :hikari-gated-admissible (boolean (:hikari-gated-admissible s))
               :care-gated-admissible (boolean (:care-gated-admissible s))
               :housing-land-grant-executed (boolean (:housing-land-grant-executed s))
               :liquidity-loan-executed (boolean (:liquidity-loan-executed s))
               :liquidity-cash-usd-micros 0
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :note "ss priority path offline — embedded in displacement scorecard"}]
      (pp/assert-no-public-scores! out)
      out)
    (catch #?(:clj Exception :cljs :default) _
      {:path "ss-offline-inkind-rails"
       :error "ss-priority-path unavailable"
       :all-rails-gated-refused true
       :all-r2-not-executed true
       :rails-gated-count 0
       :r2-status-count 0
       :r2-executed-count 0
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn r2-execute-summary
  "Facts-only R2 membrane aggregate: default refuse, executed always 0 offline."
  [pkgs]
  (let [sts (r2-statuses-from-packages pkgs)
        executed (count (filter #(true? (:executed %)) sts))
        refused (count (filter #(not (true? (:executed %))) sts))
        by-rail (frequencies
                 (mapcat (fn [p]
                           (mapcat #(keys (or (:r2-by-rail %) {}))
                                   (concat (or (:gov-subjects p) [])
                                           (or (:tenure-gov-subjects p) []))))
                         pkgs))
        out {:r2-status-count (count sts)
             :r2-refused refused
             :r2-executed executed
             :r2-by-rail by-rail
             :all-r2-not-executed (zero? executed)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "R2 execute membrane — default refuse; scaffold never side-effects"}]
    (pp/assert-no-public-scores! (dissoc out :r2-by-rail))
    out))

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
               :scorecard/housing-r1-dry
               (count (filter #(= :R1-dry (get-in % [:housing-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-gated-refused
               (count (filter #(and (:housing-gated-live-status %)
                                    (false? (get-in % [:housing-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-land-grant-executed
               (count (filter #(true? (or (get-in % [:housing-gated-live-status :land-grant-executed])
                                          (get-in % [:housing-produce-plan :land-grant-executed])
                                          (:land-grant-executed %)))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-council-held
               (count (filter #(true? (get-in % [:housing-gated-live-status :council-housing-held?]))
                              (concat subjects tenure-subjects)))
               :scorecard/tooling-r1-dry
               (count (filter #(= :R1-dry (get-in % [:tooling-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/tooling-gated-refused
               (count (filter #(and (:tooling-gated-live-status %)
                                    (false? (get-in % [:tooling-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/tooling-fulfillment-executed
               (count (filter #(true? (get-in % [:tooling-produce-plan :fulfillment-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/compute-r1-dry
               (count (filter #(= :R1-dry (get-in % [:compute-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/compute-gated-refused
               (count (filter #(and (:compute-gated-live-status %)
                                    (false? (get-in % [:compute-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/compute-quota-executed
               (count (filter #(true? (get-in % [:compute-produce-plan :quota-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-r1-dry
               (count (filter #(= :R1-dry (get-in % [:liquidity-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-gated-refused
               (count (filter #(and (:liquidity-gated-live-status %)
                                    (false? (get-in % [:liquidity-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-loan-executed
               (count (filter #(true? (or (get-in % [:liquidity-gated-live-status :loan-executed])
                                          (get-in % [:liquidity-package :loan-executed])))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-member-principal
               (count (filter #(true? (get-in % [:liquidity-package :member-principal]))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-cash-usd-micros
               (reduce + 0 (map #(or (get-in % [:liquidity-package :cash-usd-micros]) 0)
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
                     pkgs)}
         r2sum (r2-execute-summary pkgs)
         body (assoc body
                     :scorecard/r2-status-count (:r2-status-count r2sum)
                     :scorecard/r2-refused (:r2-refused r2sum)
                     :scorecard/r2-executed (:r2-executed r2sum)
                     :scorecard/r2-by-rail (:r2-by-rail r2sum)
                     :scorecard/all-r2-not-executed (:all-r2-not-executed r2sum))
         ;; Priority #2: offline all-disclosure-held stress projection (does not mutate open batch)
         stress (try
                  (when-not (= :all-held (:disclosure-stress batch))
                    (let [sb (dgov/package-batch-all-disclosure-held batch)
                          enr (filter #(= :offline-enrolled (:phase %)) (:packages sb))]
                      {:stress "all-disclosure-held"
                       :held-subjects (or (:disclosure-stress-held-subjects sb) 0)
                       :gov-flowable (long (or (:gov-flowable-committed-usd-micros sb) 0))
                       :tenure-gov-flowable
                       (long (or (:tenure-gov-flowable-committed-usd-micros sb) 0))
                       :g2-admissible-cohorts
                       (count (filter #(true? (get-in % [:couple :admissible])) enr))
                       :open-gov-flowable
                       (long (or (:gov-flowable-committed-usd-micros batch) 0))
                       :land-grant-executed 0
                       :r2-executed 0
                       :live false
                       :cash-usd-micros 0
                       :score-surface []
                       :priority-stack PRIORITY-STACK
                       :note "stress only — open path unchanged; live refuse"}))
                  (catch Exception _ nil))
         ss-path-fact (ss-priority-path-scorecard-fact)
         body (cond-> (assoc body :scorecard/ss-priority-path ss-path-fact)
                stress (assoc :scorecard/all-held-stress stress))]
     (pp/assert-no-public-scores!
      (dissoc body :scorecard/itonami-ledger :scorecard/cohorts :scorecard/live-legs
              :scorecard/tenure-stage-counts :scorecard/stage-counts
              :scorecard/gov-route-counts :scorecard/tenure-gov-route-counts
              :scorecard/r2-by-rail
              :scorecard/ss-priority-path
              :scorecard/all-held-stress))
     (when stress (pp/assert-no-public-scores! stress))
     (when-let [sp (:scorecard/ss-priority-path body)]
       (pp/assert-no-public-scores! (dissoc sp :error)))
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
                     (or (:scorecard/care-delivery-executed body) 0) "\n")
                (str "- housing-commons R1-dry / gated-refused / land-grant-executed: "
                     (or (:scorecard/housing-r1-dry body) 0) "/"
                     (or (:scorecard/housing-gated-refused body) 0) "/"
                     (or (:scorecard/housing-land-grant-executed body) 0) "\n")
                (str "- housing council-held (awaiting Lv7): "
                     (or (:scorecard/housing-council-held body) 0) "\n")
                (str "- tooling-okaimono R1-dry / gated-refused / fulfillment-executed: "
                     (or (:scorecard/tooling-r1-dry body) 0) "/"
                     (or (:scorecard/tooling-gated-refused body) 0) "/"
                     (or (:scorecard/tooling-fulfillment-executed body) 0) "\n")
                (str "- compute-murakumo R1-dry / gated-refused / quota-executed: "
                     (or (:scorecard/compute-r1-dry body) 0) "/"
                     (or (:scorecard/compute-gated-refused body) 0) "/"
                     (or (:scorecard/compute-quota-executed body) 0) "\n")
                (str "- liquidity-warifu R1-dry / gated-refused / loan-executed: "
                     (or (:scorecard/liquidity-r1-dry body) 0) "/"
                     (or (:scorecard/liquidity-gated-refused body) 0) "/"
                     (or (:scorecard/liquidity-loan-executed body) 0) "\n")
                (str "- liquidity member-principal / cash-usd-micros: "
                     (or (:scorecard/liquidity-member-principal body) 0) "/"
                     (or (:scorecard/liquidity-cash-usd-micros body) 0) "\n")
                (str "- R2 execute membrane statuses / refused / executed: "
                     (or (:scorecard/r2-status-count body) 0) "/"
                     (or (:scorecard/r2-refused body) 0) "/"
                     (or (:scorecard/r2-executed body) 0) "\n")
                (str "- all-r2-not-executed: "
                     (boolean (:scorecard/all-r2-not-executed body true)) "\n")
                (str "- r2-by-rail: " (pr-str (or (:scorecard/r2-by-rail body) {})) "\n")])]
    (when-let [sp (:scorecard/ss-priority-path body)]
      (when (and (map? sp) (not (:error sp)))
        (conj! lines "\n## SS priority path (L0 + disclosure + all rails gated)\n\n")
        (conj! lines (str "- L0 stage/published: " (:l0-stage sp) "/"
                         (boolean (:l0-published sp)) "\n"))
        (conj! lines (str "- ladder offline: " (or (:ladder-from sp) "L0") "→"
                         (or (:ladder-to sp) "—")
                         " target=" (or (:ladder-target sp) "—")
                         " steps=" (or (:ladder-steps sp) 0)
                         " rails-hint-first=" (or (:ladder-rails-hint-first sp) "—")
                         " published=" (boolean (:ladder-published sp)) "\n"))
        (conj! lines (str "- stage_sustenance: stage="
                         (or (:stage-sustenance-stage sp) "—")
                         " rails-first/second="
                         (or (:stage-rails-first sp) "—") "/"
                         (or (:stage-rails-second sp) "—")
                         " care-h/housing-mo="
                         (or (:stage-care-hours-floor-yr sp) 0) "/"
                         (or (:stage-housing-months-floor-yr sp) 0)
                         " land-grant=" (boolean (:stage-land-grant-executed sp))
                         " r2-all-refused=" (boolean (:stage-r2-all-refused sp))
                         " gated-all-refused=" (boolean (:stage-all-gated-refused sp))
                         " gated-count=" (or (:stage-gated-count sp) 0) "\n"))
        (conj! lines (str "- stage care/mitsuho/hikari gated-admissible: "
                         (boolean (:stage-care-gated-admissible sp)) "/"
                         (boolean (:stage-mitsuho-gated-admissible sp)) "/"
                         (boolean (:stage-hikari-gated-admissible sp)) "\n"))
        (conj! lines (str "- disclosure state / entitlements-may-flow: "
                         (:disclosure-state sp) "/"
                         (boolean (:entitlements-may-flow? sp)) "\n"))
        (conj! lines (str "- held-stress held / food-r1 / ladder-refused: "
                         (boolean (:held-stress-held? sp)) "/"
                         (or (:held-stress-food-phase sp) "—") "/"
                         (boolean (:held-stress-ladder-refused sp)) "\n"))
        (conj! lines (str "- rails-gated-count / admissible / all-rails-gated-refused: "
                         (or (:rails-gated-count sp) 0) "/"
                         (or (:rails-gated-admissible-count sp) 0) "/"
                         (boolean (:all-rails-gated-refused sp)) "\n"))
        (conj! lines (str "- mitsuho/hikari/care gated-admissible: "
                         (boolean (:mitsuho-gated-admissible sp)) "/"
                         (boolean (:hikari-gated-admissible sp)) "/"
                         (boolean (:care-gated-admissible sp)) "\n"))
        (conj! lines (str "- housing land-grant / liquidity loan / cash: "
                         (boolean (:housing-land-grant-executed sp)) "/"
                         (boolean (:liquidity-loan-executed sp)) "/"
                         (or (:liquidity-cash-usd-micros sp) 0) "\n"))
        (conj! lines (str "- ss R2 statuses / executed / all-not-executed: "
                         (or (:r2-status-count sp) 0) "/"
                         (or (:r2-executed-count sp) 0) "/"
                         (boolean (:all-r2-not-executed sp true)) "\n"))
        (conj! lines (str "- live: " (boolean (:live sp))
                         " cash: " (or (:cash-usd-micros sp) 0) "\n"))))
    (when-let [st (:scorecard/all-held-stress body)]
      (conj! lines "\n## All-disclosure-held stress (priority #2, offline)\n\n")
      (conj! lines (str "- stress: " (:stress st) "\n"))
      (conj! lines (str "- held subjects: " (:held-subjects st) "\n"))
      (conj! lines (str "- open-path gov flowable: " (:open-gov-flowable st) "\n"))
      (conj! lines (str "- all-held gov flowable: " (:gov-flowable st) "\n"))
      (conj! lines (str "- all-held tenure gov flowable: " (:tenure-gov-flowable st) "\n"))
      (conj! lines (str "- all-held G2 admissible cohorts: " (:g2-admissible-cohorts st) "\n"))
      (conj! lines (str "- land-grant-executed: " (:land-grant-executed st) "\n"))
      (conj! lines (str "- live: " (:live st) " cash: " (:cash-usd-micros st) "\n")))
    (conj! lines "\n## Cohorts\n\n")
    (conj! lines
           (str "| actor | cohort | phase | n | L4-flow | L4-post | ten-flow | ten-post "
                "| land-grant | headroom | tenure | tenure-n |\n"))
    (conj! lines "|---|---|---|---|---|---|---|---|---|---|---|---|\n")
    (doseq [c (:scorecard/cohorts body)]
      (conj! lines
             (str "| " (:displacing-actor c) " | " (:cohort-id c) " | "
                  (:phase c) " | " (:subjects c) " | " (:committed c) " | "
                  (or (:committed-post-ratify c)
                      (:gov-post-ratify c) 0) " | "
                  (or (:tenure-gov-flowable c) 0) " | "
                  (or (:tenure-gov-post-ratify c) 0) " | "
                  0 " | "
                  (:headroom c) " | "
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
