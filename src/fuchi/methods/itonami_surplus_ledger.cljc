(ns fuchi.methods.itonami-surplus-ledger
  "itonami_surplus_ledger.cljc — offline append-only surplus ledger for robotics/itonami.

  Projects itonami displacement seed events into a public facts ledger:
  - surplus funds Public Fund earmarks only (never cash to workers)
  - G2: unfunded surplus → refused entry
  - no personal scores; live=false; no live itonami API

  Portable .cljc; file write at #?(:clj) edge only."
  (:require [fuchi.methods.itonami-bridge :as bridge]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn ledger-entry-from-public-fact
  "One ledger line from an itonami public fact row."
  [row]
  (let [entry {:ledger/id (str "itonami-surplus/" (:displacing-actor row) "/" (:cohort-id row))
               :ledger/source "itonami-bridge-offline"
               :ledger/displacing-actor (:displacing-actor row)
               :ledger/cohort-id (:cohort-id row)
               :ledger/displaced-count (:displaced-count row)
               :ledger/surplus-usd-micros-yr (or (:surplus-usd-micros-yr row)
                                                 (:earmark-usd-micros-yr row)
                                                 0)
               :ledger/earmark-usd-micros-yr (or (:earmark-usd-micros-yr row) 0)
               :ledger/funded (boolean (:funded row))
               :ledger/admissible (boolean (:admissible row))
               :ledger/cash-to-workers-usd-micros 0
               :ledger/cash-usd-micros 0
               :ledger/live false
               :ledger/score-surface []
               :ledger/priority-stack PRIORITY-STACK
               :ledger/note (if (:admissible row)
                              "funded surplus → Public Fund earmark only"
                              "G2 refuse: displacement without funded surplus")}]
    (pp/assert-no-public-scores! entry)
    entry))

(defn build-ledger
  "itonami seed (+ optional fuchi seed) → ordered ledger entries (offline)."
  ([itonami-seed]
   (build-ledger itonami-seed nil))
  ([itonami-seed fuchi-seed]
   (let [rows (bridge/public-facts-from-itonami itonami-seed fuchi-seed)]
     (mapv ledger-entry-from-public-fact rows))))

(defn ledger-summary
  [entries]
  {:events (count entries)
   :funded-admissible (count (filter :ledger/admissible entries))
   :refused (count (remove :ledger/admissible entries))
   :total-displaced (reduce + 0 (map :ledger/displaced-count entries))
   :total-earmark-usd-micros-yr (reduce + 0 (map :ledger/earmark-usd-micros-yr entries))
   :cash-to-workers-usd-micros 0
   :live false
   :score-surface []
   :priority-stack PRIORITY-STACK})

#?(:clj
   (defn write-ledger!
     "Write out/itonami-surplus-ledger.edn from seed files. Never deploys live."
     ([]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (io/file "."))
            itonami (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
            fuchi (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
            entries (build-ledger itonami fuchi)
            summary (ledger-summary entries)
            body {:ledger/id "com.etzhayyim.fuchi.itonami-surplus-ledger"
                  :ledger/adr ["2606032130" "2607177000"]
                  :ledger/live false
                  :ledger/cash-usd-micros 0
                  :ledger/score-surface []
                  :ledger/priority-stack PRIORITY-STACK
                  :ledger/summary summary
                  :ledger/entries entries}
            outd (io/file actor "out")]
        (.mkdirs outd)
        (doseq [e entries] (pp/assert-no-public-scores! e))
        (spit (io/file outd "itonami-surplus-ledger.edn") (pr-str body))
        {:path (str (io/file outd "itonami-surplus-ledger.edn"))
         :summary summary
         :live false
         :cash-usd-micros 0
         :score-surface []
         :deployed false}))))
