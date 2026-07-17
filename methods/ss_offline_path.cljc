(ns fuchi.methods.ss-offline-path
  "ss_offline_path.cljc — end-to-end OFFLINE path for covenantal SS fragment.

  L0 enroll → disclosure open → food/energy/care/housing/tooling/compute R1 →
  dry-receive → dry produce/generate/care plans + optional itonami displacement facts.

  live=false throughout. cash≡0. no scores. Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.rail-mitsuho :as food]
            [fuchi.methods.rail-hikari :as energy]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-receive :as hrecv]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.care-iyashi-receive :as crecv]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.itonami-bridge :as itonami]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn run-food-path
  "Offline path for one subject across in-kind rails (optional micros per rail)."
  [{:keys [subject-did vow-text member-signature food-imputed-usd-micros-yr
           energy-imputed-usd-micros-yr care-imputed-usd-micros-yr
           housing-imputed-usd-micros-yr tooling-imputed-usd-micros-yr
           compute-imputed-usd-micros-yr]
    :or {food-imputed-usd-micros-yr 2000000000 energy-imputed-usd-micros-yr 0
         care-imputed-usd-micros-yr 0 housing-imputed-usd-micros-yr 0
         tooling-imputed-usd-micros-yr 0 compute-imputed-usd-micros-yr 0}}]
  (let [enrolled (l0/enroll {:subject-did subject-did
                             :vow-text (or vow-text "L0 offline path vow")
                             :member-signature (or member-signature (str "sig-" subject-did))
                             :covenant "outreach"})
        person {:did subject-did
                :covenant "vowed"
                :rails (cond-> []
                         (pos? food-imputed-usd-micros-yr) (conj {:kind "food" :active? true})
                         (pos? energy-imputed-usd-micros-yr) (conj {:kind "energy" :active? true})
                         (pos? care-imputed-usd-micros-yr) (conj {:kind "care" :active? true})
                         (pos? housing-imputed-usd-micros-yr) (conj {:kind "housing" :active? true})
                         (pos? tooling-imputed-usd-micros-yr) (conj {:kind "tooling" :active? true})
                         (pos? compute-imputed-usd-micros-yr) (conj {:kind "compute" :active? true}))
                :floor-usd-micros-yr (+ food-imputed-usd-micros-yr energy-imputed-usd-micros-yr
                                        care-imputed-usd-micros-yr housing-imputed-usd-micros-yr
                                        tooling-imputed-usd-micros-yr compute-imputed-usd-micros-yr)
                :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                             :wellbecoming-attest-fact :submitted
                             :related-party-edges [] :rider-s2-self-report :none}
                :exit-suspended? false
                :stage "L0"}
        hold (dh/initial person)
        food-pkg (when (pos? food-imputed-usd-micros-yr)
                   (food/r1-dry-package
                    {:alloc-id (str "food-" subject-did)
                     :subject-did subject-did
                     :imputed-usd-micros-yr food-imputed-usd-micros-yr
                     :person person
                     :hold-machine hold}))
        energy-pkg (when (pos? energy-imputed-usd-micros-yr)
                     (energy/r1-dry-package
                      {:alloc-id (str "energy-" subject-did)
                       :subject-did subject-did
                       :imputed-usd-micros-yr energy-imputed-usd-micros-yr
                       :person person
                       :hold-machine hold}))
        care-pkg (when (pos? care-imputed-usd-micros-yr)
                   (care/r1-dry-package
                    {:alloc-id (str "care-" subject-did)
                     :subject-did subject-did
                     :imputed-usd-micros-yr care-imputed-usd-micros-yr
                     :person person
                     :hold-machine hold}))
        housing-pkg (when (pos? housing-imputed-usd-micros-yr)
                      (housing/r1-dry-package
                       {:alloc-id (str "housing-" subject-did)
                        :subject-did subject-did
                        :imputed-usd-micros-yr housing-imputed-usd-micros-yr
                        :person person
                        :hold-machine hold}))
        tooling-pkg (when (pos? tooling-imputed-usd-micros-yr)
                      (tooling/r1-dry-package
                       {:alloc-id (str "tooling-" subject-did)
                        :subject-did subject-did
                        :imputed-usd-micros-yr tooling-imputed-usd-micros-yr
                        :person person
                        :hold-machine hold}))
        compute-pkg (when (pos? compute-imputed-usd-micros-yr)
                      (compute/r1-dry-package
                       {:alloc-id (str "compute-" subject-did)
                        :subject-did subject-did
                        :imputed-usd-micros-yr compute-imputed-usd-micros-yr
                        :person person
                        :hold-machine hold}))
        food-plan (when (and food-pkg (not= :refused (:phase food-pkg)))
                    (mprod/plan-from-r1 food-pkg))
        energy-plan (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                      (hprod/plan-from-r1 energy-pkg))
        care-plan (when (and care-pkg (not= :refused (:phase care-pkg)))
                    (cprod/plan-from-r1 care-pkg))
        care-ack (when (and care-pkg (not= :refused (:phase care-pkg)))
                   (crecv/receive-from-r1-package care-pkg))
        out {:path "ss-offline-inkind-rails"
             :priority-stack PRIORITY-STACK
             :live false
             :cash-usd-micros 0
             :score-surface []
             :l0 enrolled
             :disclosure-hold hold
             :public-person (pp/public-surface person :stage "L0")
             :food-package food-pkg
             :food-produce-plan food-plan
             :energy-package energy-pkg
             :energy-produce-plan energy-plan
             :energy-receive (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                               (hrecv/receive-from-r1-package energy-pkg))
             :care-package care-pkg
             :care-produce-plan care-plan
             :care-receive care-ack
             :housing-package housing-pkg
             :tooling-package tooling-pkg
             :compute-package compute-pkg}]
    (pp/assert-no-public-scores! (:public-person out))
    out))

#?(:clj
   (defn run-with-itonami-seed
     "Offline path + itonami displacement public facts."
     [opts]
     (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                     (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
           itonami-seed (edn/load-edn (io/file actor "data" "itonami-displacement-events.edn"))
           fuchi-seed (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
           path (run-food-path opts)
           disp (itonami/public-facts-from-itonami itonami-seed fuchi-seed)]
       (assoc path :itonami-displacement disp :live false :cash-usd-micros 0))))
