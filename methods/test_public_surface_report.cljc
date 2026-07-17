(ns fuchi.methods.test-public-surface-report
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.public-surface-report :as rep]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

#?(:clj
   (def ^:private seed
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))]
       (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn")))))

#?(:clj
   (deftest test-seed-facts-no-scores
     (let [facts (rep/seed-public-facts seed)]
       (is (seq facts))
       (doseq [f facts]
         (is (nil? (:priority-rank f)))
         (is (= [] (:score-surface f)))
         (pp/assert-no-public-scores! f)))))

#?(:clj
   (deftest test-report-md-has-no-rank-columns
     (let [md (rep/report-md seed :include-l0-demo true)]
       (is (str/includes? md "public surface"))
       (is (str/includes? md "wellbecoming"))
       (is (not (str/includes? md "| rank |")))
       (is (not (re-find #"(?i)\| *percentile *\|" md)))
       (is (str/includes? md "No personal scores"))
       (is (str/includes? md "L0 demo")))))

#?(:clj
   (deftest test-report-edn-invariants
     (let [body (rep/report-edn seed :include-l0-demo true)]
       (is (= [] (:report/score-surface body)))
       (is (= 0 (:report/cash-usd-micros body)))
       (is (false? (:report/live body)))
       (is (= pp/PRIORITY-STACK (:report/priority-stack body)))
       (is (seq (:report/rail-packages body)))
       (is (seq (:report/displacement body)))
       (is (= 2 (get-in body [:report/displacement-summary :displacement-events]))))))

#?(:clj
   (deftest test-write-report
     (let [paths (rep/write-report!)]
       (is (.exists (io/file (:md paths))))
       (is (.exists (io/file (:edn paths))))
       (is (.exists (io/file (:html paths))))
       (is (str/includes? (slurp (:md paths)) "facts only"))
       (is (str/includes? (slurp (:html paths)) "Displacement"))
       (is (not (re-find #"(?i)\| *rank *\|" (slurp (:html paths))))))))

#?(:clj
   (deftest test-html-no-scores
     (let [html (rep/report-html seed)]
       (is (str/includes? html "public surface"))
       (is (str/includes? html "wellbecoming"))
       (is (str/includes? html "sanae"))
       (is (false? (str/includes? html "priority-rank"))))))
