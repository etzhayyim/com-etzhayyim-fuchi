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
     (let [md (rep/report-md seed :include-l0-demo true :include-itonami true)]
       (is (str/includes? md "public surface"))
       (is (str/includes? md "wellbecoming"))
       (is (str/includes? md "Dry floor facts"))
       (is (str/includes? md "care-h"))
       (is (not (str/includes? md "| rank |")))
       (is (not (re-find #"(?i)\| *percentile *\|" md)))
       (is (str/includes? md "No personal scores"))
       (is (str/includes? md "L0 demo")))))

#?(:clj
   (deftest test-report-edn-invariants
     (let [body (rep/report-edn seed :include-l0-demo true :include-itonami true)]
       (is (= [] (:report/score-surface body)))
       (is (= 0 (:report/cash-usd-micros body)))
       (is (false? (:report/live body)))
       (is (= pp/PRIORITY-STACK (:report/priority-stack body)))
       (is (seq (:report/rail-packages body)))
       (is (seq (:report/displacement body)))
       (is (= 2 (get-in body [:report/displacement-summary :displacement-events])))
       ;; multi-rail floors present for seed maintainers with food/energy/etc
       (let [abel (first (filter #(str/includes? (str (:did %)) "abel")
                                 (:report/rail-packages body)))]
         (is (some? abel))
         (is (or (nil? (:food-floor abel))
                 (false? (get-in abel [:food-floor :produce-executed]))))
         (when-let [ff (:food-floor abel)]
           (is (pos? (:kcal-floor-yr ff))))
         (when-let [ef (:energy-floor abel)]
           (is (pos? (:kwh-floor-yr ef))))
         (is (= 0 (:cash-usd-micros abel)))
         (is (= [] (:score-surface abel))))
       (is (seq (:report/itonami-displacement body)))
       (is (= 4 (count (:report/itonami-displacement body))))
       (is (map? (:report/displacement-l0 body)))
       (is (pos? (get-in body [:report/displacement-l0 :admissible-cohorts] 0)))
       (is (pos? (get-in body [:report/displacement-l0 :refused-cohorts] 0)))
       (is (= 0 (get-in body [:report/displacement-l0 :cash-usd-micros])))
       (is (= [] (get-in body [:report/displacement-l0 :score-surface])))
       (is (true? (get-in body [:report/displacement-scorecard :scorecard/all-live-refused])))
       (is (pos? (get-in body [:report/displacement-scorecard :scorecard/enrolled-subjects] 0)))
       (is (pos? (get-in body [:report/displacement-scorecard :scorecard/liquidity-r1-dry] 0)))
       (is (pos? (get-in body [:report/displacement-scorecard :scorecard/liquidity-member-principal] 0)))
       (is (zero? (get-in body [:report/displacement-scorecard :scorecard/liquidity-loan-executed] 0)))
       (is (zero? (get-in body [:report/displacement-scorecard :scorecard/liquidity-cash-usd-micros] 0)))
       (is (zero? (get-in body [:report/displacement-scorecard :scorecard/housing-land-grant-executed] 0)))
       (is (map? (get-in body [:report/displacement-scorecard :scorecard/all-held-stress]))))))

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
   (deftest test-public-md-html-liquidity-and-rails
     (let [md (rep/report-md seed :include-l0-demo true :include-itonami true)
           html (rep/report-html seed :include-l0-demo true :include-itonami true)
           body (rep/report-edn seed :include-l0-demo true :include-itonami true)]
       (is (str/includes? md "liquidity-warifu"))
       (is (str/includes? md "member-principal"))
       (is (str/includes? md "All-disclosure-held stress"))
       (is (str/includes? md "housing-commons"))
       (is (str/includes? md "care-iyashi"))
       (is (str/includes? md "disc-o/h"))
       (is (str/includes? md "disclosure-open="))
       (is (str/includes? md "| g2 |"))
       (is (str/includes? md "| earmark |"))
       (is (str/includes? md "earmark-total="))
       (is (str/includes? md "committed-flowable-total="))
       (is (str/includes? md "committed-full-total="))
       (is (str/includes? md "tenure-subjects="))
       (is (str/includes? md "tenure-committed-flow="))
       (is (str/includes? md "ten-flow"))
       (is (str/includes? md "Pages deploy"))
       (is (str/includes? md "default refuse"))
       (is (str/includes? html "liquidity-warifu"))
       (is (str/includes? html "member-principal"))
       (is (str/includes? html "All-disclosure-held stress"))
       (is (str/includes? html "land-grant-executed"))
       (is (str/includes? html "cash-usd-micros"))
       (is (str/includes? html ">g2<"))
       (is (str/includes? html ">earmark<"))
       (is (str/includes? html "L4-flow"))
       (is (str/includes? html "ten-flow"))
       (is (str/includes? html "tenure-subjects="))
       (is (str/includes? html "earmark-total="))
       (is (str/includes? html "Pages deploy"))
       (is (= :refused (get-in body [:report/pages-deploy-status :phase])))
       (is (false? (get-in body [:report/pages-deploy-status :deployed])))
       (is (false? (get-in body [:report/pages-deploy-status :live])))
       (is (= 0 (get-in body [:report/pages-deploy-status :cash-usd-micros])))
       (let [pkgs (get-in body [:report/displacement-l0 :packages])
             enrolled (filter #(= "offline-enrolled" (:phase %)) pkgs)
             refused (filter #(= "refused" (:phase %)) pkgs)
             dl0 (:report/displacement-l0 body)]
         (is (seq enrolled))
         (is (seq refused))
         (is (every? true? (map :g2-admissible enrolled)))
         (is (every? false? (map :g2-admissible refused)))
         (is (every? true? (map :funded enrolled)))
         (is (every? false? (map :funded refused)))
         (is (every? pos? (map :earmark-usd-micros-yr enrolled)))
         (is (every? zero? (map :earmark-usd-micros-yr refused)))
         (is (pos? (:earmark-usd-micros-yr dl0)))
         (is (pos? (:committed-usd-micros-yr dl0)))
         (is (pos? (:committed-full-usd-micros-yr dl0)))
         ;; housing held under Council → flowable committed strictly below full book
         (is (every? #(<= (:committed-usd-micros-yr %)
                          (:committed-full-usd-micros-yr %))
                     enrolled))
         (is (some #(< (:committed-usd-micros-yr %)
                       (:committed-full-usd-micros-yr %))
                   enrolled))
         (is (< (:committed-usd-micros-yr dl0)
                (:committed-full-usd-micros-yr dl0)))
         (is (pos? (:gov-flowable-usd-micros dl0)))
         (is (= (count enrolled) (:g2-admissible-cohorts dl0)))
         (is (pos? (:tenure-subjects dl0)))
         (is (pos? (:tenure-g2-cohorts dl0)))
         (is (pos? (:tenure-committed-usd-micros-yr dl0)))
         (is (pos? (:tenure-gov-flowable-usd-micros dl0)))
         (is (every? #(= "tenure-offline" (:tenure-phase %)) enrolled))
         (is (every? pos? (map :tenure-subjects enrolled))))
       (is (not (str/includes? md "| rank |")))
       (is (not (re-find #"(?i)percentile" html))))))

#?(:clj
   (deftest test-html-no-scores
     (let [html (rep/report-html seed)]
       (is (str/includes? html "public surface"))
       (is (str/includes? html "wellbecoming"))
       (is (str/includes? html "sanae"))
       (is (false? (str/includes? html "priority-rank"))))))
