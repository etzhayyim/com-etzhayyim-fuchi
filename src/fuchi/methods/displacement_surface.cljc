(ns fuchi.methods.displacement-surface
  "displacement_surface.cljc — public facts for robotics/itonami labor displacement
  → TitheRouter 10% → cohort earmark (ADR-2606032130 / 2607177000).

  Bridges cloud-itonami style displacement events to covenantal SS funding facts.
  No personal scores. cash≡0 on recipient side. G2: unfunded cohort refused.
  Portable .cljc."
  (:require [clojure.string :as str]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.public-person :as pp]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["displacement-surplus-funds-descendant-sustenance-floor"
   "no-live-displacement-without-funded-cohort-G2"
   "not-a-worker-ranking-score"
   "displaced-count-is-cohort-fact-not-score"])

(defn- maintainer-floor-by-actor
  "Sum prior-imputed floors for maintainers who maintain a given actor handle."
  [seed actor]
  (let [actor (str actor)
        recs (get seed ":maintainer/batch" [])]
    (reduce
     (fn [acc r]
       (let [ms (map str (or (get r ":maintainer/maintains") []))]
         (if (some #{actor} ms)
           (+ acc (long (get r ":maintainer/prior-imputed-usd-micros-yr" 0)))
           acc)))
     0
     recs)))

(defn public-displacement-facts
  "Facts-only displacement × earmark × G2 gate rows from seed :cohort/displacement."
  [seed]
  (let [events (couple/events-from-seed (get seed ":cohort/displacement" []))]
    (mapv
     (fn [ev]
       (let [em (couple/earmark-from-surplus ev)
             committed (maintainer-floor-by-actor seed (:displacing-actor ev))
             g (couple/coupling-gate ev em committed)
             row {:displacing-actor (:displacing-actor ev)
                  :cohort-id (:cohort-id ev)
                  :displaced-count (:displaced-count ev)
                  :surplus-usd-micros-yr (:surplus-usd-micros-yr ev)
                  :tithe-usd-micros (:tithe-usd-micros em)
                  :earmark-usd-micros-yr (:earmark-usd-micros-yr em)
                  :funded (:funded em)
                  :committed-floor-usd-micros-yr committed
                  :admissible (boolean (get g "admissible"))
                  :gate-reason (get g "reason")
                  :headroom (get g "headroom")
                  :cash-usd-micros 0
                  :live false
                  :priority-stack PRIORITY-STACK
                  :multi-gen-facts MULTI-GEN-FACTS
                  :score-surface []}]
         (pp/assert-no-public-scores! row)
         row))
     events)))

(defn summary
  "Aggregate facts only (counts, not rankings)."
  [facts]
  {:displacement-events (count facts)
   :funded-admissible (count (filter :admissible facts))
   :refused (count (filter (complement :admissible) facts))
   :total-displaced (reduce + 0 (map :displaced-count facts))
   :total-earmark-usd-micros-yr (reduce + 0 (map :earmark-usd-micros-yr facts))
   :cash-usd-micros 0
   :score-surface []
   :priority-stack PRIORITY-STACK
   :multi-gen-facts MULTI-GEN-FACTS})
