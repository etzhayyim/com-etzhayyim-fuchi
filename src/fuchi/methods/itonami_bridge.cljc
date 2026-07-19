(ns fuchi.methods.itonami-bridge
  "itonami_bridge.cljc — map cloud-itonami / robotics displacement events into
  fuchi Displacement-Dividend couple + public facts (ADR-2606032130 / 2607177000).

  Offline ingest only. No live itonami API. No cash to workers. No personal scores.
  Portable .cljc."
  (:require [clojure.string :as str]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.displacement-surface :as disp]
            [fuchi.methods.public-person :as pp]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- g [rec & ks]
  (some (fn [k]
          (let [v (get rec k)]
            (when (or (some? v) (false? v) (zero? (if (number? v) v 1)))
              (when (contains? rec k) v))))
        ks))

(defn- lookup [rec ks]
  (loop [ks ks]
    (when (seq ks)
      (let [k (first ks)]
        (if (contains? rec k)
          (get rec k)
          (recur (rest ks)))))))

(defn itonami->couple-event
  "Convert one :itonami.displacement/* record (string- or keyword-keyed) to couple event."
  [rec]
  (let [actor (str (or (lookup rec [":itonami.displacement/actor-id" :itonami.displacement/actor-id
                                    ":actor-id" :actor-id])
                       "?"))
        cohort (str (or (lookup rec [":itonami.displacement/cohort-id" :itonami.displacement/cohort-id
                                     ":cohort-id" :cohort-id])
                        (str "cohort-" actor)))
        n (long (or (lookup rec [":itonami.displacement/displaced-worker-count"
                                 :itonami.displacement/displaced-worker-count
                                 ":displaced-count" :displaced-count])
                    0))
        surplus (long (or (lookup rec [":itonami.displacement/surplus-usd-micros-yr"
                                       :itonami.displacement/surplus-usd-micros-yr
                                       ":surplus-usd-micros-yr" :surplus-usd-micros-yr])
                          0))
        funded (boolean (lookup rec [":itonami.displacement/surplus-landed-in-public-fund"
                                     :itonami.displacement/surplus-landed-in-public-fund
                                     ":funded" :funded]))]
    (couple/make-displacement-event
     {:displacing-actor actor
      :cohort-id cohort
      :displaced-count n
      :surplus-usd-micros-yr surplus
      :funded funded})))

(defn load-itonami-batch
  "Read data/itonami-displacement-events.edn → vector of couple events."
  [seed-or-path]
  #?(:clj
     (let [batch (if (map? seed-or-path)
                   (or (get seed-or-path ":events/batch")
                       (get seed-or-path :events/batch)
                       [])
                   (let [m (edn/load-edn seed-or-path)]
                     (or (get m ":events/batch") [])))]
       (mapv itonami->couple-event batch))
     :cljs
     (mapv itonami->couple-event
           (or (get seed-or-path ":events/batch")
               (get seed-or-path :events/batch)
               []))))

#?(:clj
   (defn load-itonami-seed-file
     []
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (io/file "."))]
       (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn")))))

(defn public-facts-from-itonami
  "itonami events + optional fuchi sustenance seed for committed floors → public facts."
  ([itonami-seed]
   (public-facts-from-itonami itonami-seed nil))
  ([itonami-seed fuchi-seed]
   (let [events (if (sequential? itonami-seed)
                  (mapv itonami->couple-event itonami-seed)
                  (load-itonami-batch itonami-seed))
         ;; reuse displacement-surface shape via synthetic seed
         synth {":cohort/displacement"
                (mapv (fn [ev]
                        {":event/displacing-actor" (:displacing-actor ev)
                         ":event/cohort-id" (:cohort-id ev)
                         ":event/displaced-count" (:displaced-count ev)
                         ":event/surplus-usd-micros-yr" (:surplus-usd-micros-yr ev)
                         ":event/funded" (:funded ev)})
                      events)
                ":maintainer/batch" (or (get fuchi-seed ":maintainer/batch") [])}
         rows (disp/public-displacement-facts synth)]
     (mapv (fn [r]
             (let [row (assoc r
                              :source "itonami-bridge"
                              :cash-usd-micros 0
                              :live false
                              :score-surface []
                              :priority-stack PRIORITY-STACK)]
               (pp/assert-no-public-scores! row)
               row))
           rows))))
