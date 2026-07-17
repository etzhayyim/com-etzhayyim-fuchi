(ns fuchi.methods.test-displacement-surface
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.displacement-surface :as d]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

#?(:clj
   (def ^:private seed
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))]
       (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn")))))

#?(:clj
   (deftest test-seed-displacement-facts
     (let [rows (d/public-displacement-facts seed)
           by (into {} (map (fn [r] [(:displacing-actor r) r]) rows))]
       (is (= 2 (count rows)))
       (is (true? (:funded (get by "sanae"))))
       (is (true? (:admissible (get by "sanae"))))
       (is (false? (:funded (get by "hataori"))))
       (is (false? (:admissible (get by "hataori"))))
       (is (= 0 (:cash-usd-micros (get by "sanae"))))
       (is (= [] (:score-surface (get by "sanae"))))
       (doseq [r rows] (pp/assert-no-public-scores! r)))))

#?(:clj
   (deftest test-summary
     (let [s (d/summary (d/public-displacement-facts seed))]
       (is (= 2 (:displacement-events s)))
       (is (= 1 (:funded-admissible s)))
       (is (= 1 (:refused s)))
       (is (= 0 (:cash-usd-micros s)))
       (is (= [] (:score-surface s))))))
