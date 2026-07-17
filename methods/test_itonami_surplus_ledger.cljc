(ns fuchi.methods.test-itonami-surplus-ledger
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.itonami-surplus-ledger :as led]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

#?(:clj
   (deftest test-build-ledger-g2
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           itonami (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           fuchi (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
           entries (led/build-ledger itonami fuchi)
           by (into {} (map (fn [e] [(:ledger/displacing-actor e) e]) entries))
           sum (led/ledger-summary entries)]
       (is (= 4 (count entries)))
       (is (true? (:ledger/admissible (get by "sanae"))))
       (is (true? (:ledger/admissible (get by "itonami-robotics"))))
       (is (false? (:ledger/admissible (get by "hataori"))))
       (is (false? (:ledger/admissible (get by "warehouse-amr"))))
       (is (= 0 (:cash-to-workers-usd-micros sum)))
       (is (= 0 (:ledger/cash-to-workers-usd-micros (get by "sanae"))))
       (is (false? (:live sum)))
       (is (= [] (:score-surface sum)))
       (doseq [e entries] (pp/assert-no-public-scores! e)))))

#?(:clj
   (deftest test-write-ledger
     (let [paths (led/write-ledger!)]
       (is (.exists (io/file (:path paths))))
       (is (false? (:live paths)))
       (is (= 0 (:cash-usd-micros paths)))
       (is (false? (:deployed paths)))
       (is (= 4 (get-in paths [:summary :events]))))))
