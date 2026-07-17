(ns fuchi.methods.test-disclosure-hold
  "Disclosure hold state machine tests (ADR-2607177000)."
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.edn :as edn])))

(def ^:private fresh
  {:wage-labor-band "0-10h"
   :state-benefits? false
   :wellbecoming-attest-fact :submitted
   :related-party-edges []
   :rider-s2-self-report :none})

(def ^:private stale
  {:wage-labor-band :stale
   :state-benefits? false
   :wellbecoming-attest-fact :stale
   :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:abel"
   :covenant "vowed"
   :rails [{:kind "food" :active? true}]
   :floor-usd-micros-yr 1000
   :disclosure d
   :exit-suspended? false})

(deftest test-initial-open-when-fresh
  (let [m (dh/initial (person fresh))]
    (is (= :open (:state m)))
    (is (false? (:entitlements-held? m)))))

(deftest test-stale-holds
  (let [m0 (dh/initial (person fresh))
        m1 (dh/transition m0 :stale-detected :reason "quarter-elapsed")]
    (is (= :held (:state m1)))
    (is (true? (:entitlements-held? m1)))
    (is (= 1 (count (:history m1))))))

(deftest test-redisclose-lifts
  (let [m0 (dh/initial (person fresh))
        m1 (dh/transition m0 :stale-detected)
        m2 (dh/transition m1 :redisclose :person (person fresh) :disclosure fresh)]
    (is (= :open (:state m2)))
    (is (false? (:entitlements-held? m2)))))

(deftest test-redisclose-stale-stays-held
  (let [m0 (dh/initial (person fresh))
        m1 (dh/transition m0 :stale-detected)
        m2 (dh/transition m1 :redisclose :person (person stale) :disclosure stale)]
    (is (= :held (:state m2)))))

(deftest test-exit-and-reaffirm
  (let [m0 (dh/initial (person fresh))
        m1 (dh/transition m0 :exit)
        m2 (dh/transition m1 :re-affirm :person (person fresh))]
    (is (= :exit-suspended (:state m1)))
    (is (false? (:public-person? m1)))
    (is (= :open (:state m2)))))

(deftest test-no-score-surface
  (is (= [] (:score-surface (dh/initial (person fresh)))))
  (is (= pp/PRIORITY-STACK (:priority-stack (dh/initial (person fresh))))))

#?(:clj
   (deftest test-seed-noah-hold-machine
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           seed (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
           surfs (pp/persons-from-seed seed)
           noah-surf (first (filter #(clojure.string/ends-with? (str (:did %)) "noah") surfs))
           rec (first (filter #(clojure.string/ends-with? (str (get % ":maintainer/did")) "noah")
                              (get seed ":maintainer/batch")))
           drec (pp/disclosure-for-did seed (get rec ":maintainer/did"))
           person (pp/persons-from-seed-row rec
                                            (filterv #(= (get % ":envelope/maintainer")
                                                         (get rec ":maintainer/did"))
                                                     (get seed ":envelope/batch" []))
                                            drec)
           m (dh/from-seed-person person)]
       (is (= :held (:state m)) (str "noah should start held, got " m))
       (is (true? (:entitlements-held? m)))
       (is (= :hold (get-in noah-surf [:disclosure-gate :action]))))))
