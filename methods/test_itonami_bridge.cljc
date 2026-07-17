(ns fuchi.methods.test-itonami-bridge
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.itonami-bridge :as b]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

#?(:clj
   (deftest test-itonami-seed-to-public-facts
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           itonami (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           fuchi (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
           rows (b/public-facts-from-itonami itonami fuchi)
           by (into {} (map (fn [r] [(:displacing-actor r) r]) rows))]
       (is (= 4 (count rows)))
       (is (true? (:admissible (get by "sanae"))))
       (is (true? (:admissible (get by "itonami-robotics"))))
       (is (false? (:admissible (get by "hataori"))))
       (is (false? (:admissible (get by "warehouse-amr"))))
       (is (= "itonami-bridge" (:source (get by "sanae"))))
       (is (= 0 (:cash-usd-micros (get by "sanae"))))
       (is (= [] (:score-surface (get by "sanae"))))
       (doseq [r rows] (pp/assert-no-public-scores! r)))))

#?(:clj
   (deftest test-event-mapping
     (let [ev (b/itonami->couple-event
               {":itonami.displacement/actor-id" "sanae"
                ":itonami.displacement/cohort-id" "c1"
                ":itonami.displacement/displaced-worker-count" 3
                ":itonami.displacement/surplus-usd-micros-yr" 1000
                ":itonami.displacement/surplus-landed-in-public-fund" true})]
       (is (= "sanae" (:displacing-actor ev)))
       (is (= 3 (:displaced-count ev)))
       (is (true? (:funded ev))))))
