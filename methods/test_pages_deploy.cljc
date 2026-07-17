(ns fuchi.methods.test-pages-deploy
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.pages-deploy :as dep]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])))

(deftest test-default-refuse
  (let [st (dep/default-refuse-status {})
        refu (dep/refuse-deploy {})
        rb (:operator-runbook refu)]
    (is (false? (get st "admissible")))
    (is (= :refused (:phase refu)))
    (is (false? (:deployed refu)))
    (is (false? (:authorized-to-deploy refu)))
    (is (true? (:package-ready refu)))
    (is (false? (:wrangler-invoked refu)))
    (is (false? (:live refu)))
    (is (= 0 (:cash-usd-micros refu)))
    (is (map? rb))
    (is (false? (:scaffold-invokes-wrangler rb)))
    (is (seq (:steps rb)))
    (pp/assert-no-public-scores! (dissoc refu :operator-runbook))
    (pp/assert-no-public-scores! rb)))

(deftest test-operator-runbook-facts
  (let [rb (dep/operator-runbook-facts)]
    (is (= "FUCHI_ALLOW_PAGES_DEPLOY" (:flag rb)))
    (is (false? (:scaffold-invokes-wrangler rb)))
    (is (false? (:scaffold-invokes-cloudflare-api rb)))
    (is (false? (:deployed rb)))
    (is (false? (:live-disbursement rb)))
    (is (= 0 (:cash-usd-micros rb)))
    (is (seq (:steps rb)))
    (pp/assert-no-public-scores! rb)))

(deftest test-gated-plan-not-deployed
  (let [plan (dep/gated-deploy-plan
              {:operator-did "did:op:pages" :project-name "fuchi-public-surface"}
              :env {"FUCHI_ALLOW_PAGES_DEPLOY" "1"})]
    (is (= :gated-deploy-plan (:phase plan)))
    (is (true? (:authorized-to-deploy plan)))
    (is (true? (:package-ready plan)))
    (is (false? (:deployed plan)))
    (is (false? (:wrangler-invoked plan)))
    (is (false? (:cloudflare-api-invoked plan)))
    (is (map? (:operator-runbook plan)))
    (pp/assert-no-public-scores! (dissoc plan :operator-runbook))))

(deftest test-deploy-or-refuse-no-flag
  (let [out (dep/deploy-or-refuse {:operator-did "did:op:x"} :env {})]
    (is (= :refused (:phase out)))
    (is (false? (:deployed out)))))

(deftest test-gated-deploy-status-default-refuse-no-throw
  (let [st (dep/gated-deploy-status {:operator-did "did:op:x"} :env {})]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:deployed st)))
    (is (false? (:wrangler-invoked st)))
    (is (false? (:cloudflare-api-invoked st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (pp/assert-no-public-scores! st)))

(deftest test-gated-deploy-status-plan-with-flag
  (let [st (dep/gated-deploy-status
            {:operator-did "did:op:pages" :project-name "fuchi-public-surface"}
            :env {"FUCHI_ALLOW_PAGES_DEPLOY" "1"})]
    (is (= :gated-deploy-plan (:phase st)))
    (is (true? (:admissible st)))
    (is (true? (:authorized-to-deploy st)))
    (is (false? (:deployed st)))
    (is (false? (:wrangler-invoked st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (pp/assert-no-public-scores! st)))

#?(:clj
   (deftest test-write-deploy-package
     (let [pkg (dep/write-deploy-package! {})
           st (:deploy-status pkg)
           rb-path (:deploy-runbook pkg)]
       (is (.exists (io/file (:index pkg))))
       (is (.exists (io/file (:wrangler pkg))))
       (is (.exists (io/file rb-path)))
       (is (false? (:deployed pkg)))
       (is (false? (:wrangler-invoked pkg)))
       (is (true? (:package-ready pkg)))
       (is (false? (:live pkg)))
       (is (= 0 (:cash-usd-micros pkg)))
       (is (= :refused (:phase st)))
       (is (false? (:authorized-to-deploy st)))
       (is (map? (:operator-runbook st)))
       (is (map? (:audit-snapshot st)))
       (is (false? (boolean (get-in st [:audit-snapshot :any-land-grant-executed?]))))
       (is (zero? (or (get-in st [:audit-snapshot :last-run-housing-land-grant-executed]) 0)))
       (is (false? (get-in st [:audit-snapshot :live])))
       (let [html (slurp (:index pkg))
             readme (slurp (io/file "public/README.md"))
             rb (read-string (slurp rb-path))]
         (is (str/includes? html "public surface"))
         (is (str/includes? html "wellbecoming"))
         (is (str/includes? html "Displacement → L0"))
         (is (str/includes? html "plan-only"))
         (is (str/includes? html "wrangler-invoked="))
         (is (str/includes? readme "out-of-band"))
         (is (str/includes? readme "Operator runbook"))
         (is (false? (:scaffold-invokes-wrangler rb)))
         (is (not (re-find #"(?i)priority-rank" html)))))))
