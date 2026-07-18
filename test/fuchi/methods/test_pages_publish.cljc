(ns fuchi.methods.test-pages-publish
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [fuchi.methods.pages-publish :as pages])))

#?(:clj
   (deftest test-write-pages-package
     (let [paths (pages/write-pages!)]
       (is (.exists (io/file (:index paths))))
       (is (.exists (io/file (:facts paths))))
       (is (false? (:deployed paths)))
       (is (false? (:live paths)))
       (is (= 0 (:cash-usd-micros paths)))
       (is (= [] (:score-surface paths)))
       (is (true? (:all-live-refused paths)))
       (is (.exists (io/file (:scorecard paths))))
       (is (.exists (io/file (:audit-summary paths))))
       (is (>= (:audit-runs paths) 0))
       (let [html (slurp (:index paths))
             audit (read-string (slurp (:audit-summary paths)))]
         (is (str/includes? html "public surface"))
         (is (str/includes? html "wellbecoming"))
         (is (str/includes? html "L0→L4"))
         (is (str/includes? html "SS scorecard"))
         (is (str/includes? html "Pipeline audit summary"))
         (is (str/includes? html "gov-post-ratify="))
         (is (contains? audit :last-run-gov-post-ratify-committed-usd-micros))
         (is (contains? audit :last-run-housing-land-grant-executed))
         (is (false? (boolean (:any-land-grant-executed? audit))))
         (is (zero? (or (:last-run-housing-land-grant-executed audit) 0)))
         (is (false? (boolean (:live audit))))
         (is (zero? (or (:cash-usd-micros audit) 0)))
         (is (not (re-find #"(?i)\| *rank *\|" html)))))))
