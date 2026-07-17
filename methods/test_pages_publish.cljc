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
       (let [html (slurp (:index paths))]
         (is (str/includes? html "public surface"))
         (is (str/includes? html "wellbecoming"))
         (is (str/includes? html "L0→L4"))
         (is (str/includes? html "SS scorecard"))
         (is (not (re-find #"(?i)\| *rank *\|" html)))))))
