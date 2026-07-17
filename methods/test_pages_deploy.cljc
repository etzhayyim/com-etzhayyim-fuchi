(ns fuchi.methods.test-pages-deploy
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.pages-deploy :as dep]
            [fuchi.methods.public-person :as pp]
            #?(:clj [clojure.java.io :as io])))

(deftest test-default-refuse
  (let [st (dep/default-refuse-status {})
        refu (dep/refuse-deploy {})]
    (is (false? (get st "admissible")))
    (is (= :refused (:phase refu)))
    (is (false? (:deployed refu)))
    (is (false? (:live refu)))
    (is (= 0 (:cash-usd-micros refu)))
    (pp/assert-no-public-scores! refu)))

(deftest test-gated-plan-not-deployed
  (let [plan (dep/gated-deploy-plan
              {:operator-did "did:op:pages" :project-name "fuchi-public-surface"}
              :env {"FUCHI_ALLOW_PAGES_DEPLOY" "1"})]
    (is (= :gated-deploy-plan (:phase plan)))
    (is (true? (:authorized-to-deploy plan)))
    (is (false? (:deployed plan)))
    (is (false? (:wrangler-invoked plan)))
    (is (false? (:cloudflare-api-invoked plan)))
    (pp/assert-no-public-scores! plan)))

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
     (let [pkg (dep/write-deploy-package! {})]
       (is (.exists (io/file (:index pkg))))
       (is (.exists (io/file (:wrangler pkg))))
       (is (false? (:deployed pkg)))
       (is (false? (:live pkg)))
       (is (= 0 (:cash-usd-micros pkg)))
       (is (= :refused (get-in pkg [:deploy-status :phase])))
       (let [html (slurp (:index pkg))]
         (is (str/includes? html "public surface"))
         (is (str/includes? html "wellbecoming"))
         (is (str/includes? html "Displacement → L0"))
         (is (not (re-find #"(?i)priority-rank" html)))))))
