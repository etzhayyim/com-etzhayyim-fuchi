(ns fuchi.methods.ss-offline-path
  "ss_offline_path.cljc — end-to-end OFFLINE path for covenantal SS fragment.

  L0 enroll → disclosure open → food R1 → mitsuho dry-receive → dry produce plan
  + optional itonami displacement facts.

  live=false throughout. cash≡0. no scores. Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.rail-mitsuho :as food]
            [fuchi.methods.rail-hikari :as energy]
            [fuchi.methods.mitsuho-receive :as mrecv]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-receive :as hrecv]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.itonami-bridge :as itonami]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn run-food-path
  "Offline path for one subject with food imputed micros."
  [{:keys [subject-did vow-text member-signature food-imputed-usd-micros-yr energy-imputed-usd-micros-yr]
    :or {food-imputed-usd-micros-yr 2000000000 energy-imputed-usd-micros-yr 0}}]
  (let [enrolled (l0/enroll {:subject-did subject-did
                             :vow-text (or vow-text "L0 offline path vow")
                             :member-signature (or member-signature (str "sig-" subject-did))
                             :covenant "outreach"})
        person {:did subject-did
                :covenant "vowed"
                :rails (cond-> []
                         (pos? food-imputed-usd-micros-yr) (conj {:kind "food" :active? true})
                         (pos? energy-imputed-usd-micros-yr) (conj {:kind "energy" :active? true}))
                :floor-usd-micros-yr (+ food-imputed-usd-micros-yr energy-imputed-usd-micros-yr)
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
        food-plan (when (and food-pkg (not= :refused (:phase food-pkg)))
                    (mprod/plan-from-r1 food-pkg))
        energy-ack (when (and energy-pkg (not= :refused (:phase energy-pkg)))
                     (hrecv/receive-from-r1-package energy-pkg))
        out {:path "ss-offline-food-energy"
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
             :energy-receive energy-ack}]
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
