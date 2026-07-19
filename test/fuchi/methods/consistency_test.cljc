(ns fuchi.methods.consistency-test
  "test_consistency.py — SSoT drift-lock tests for 扶持 (fuchi): manifest ↔ files ↔ ontology
  ↔ seed. 1:1 Clojure port of methods/test_consistency.py (stdlib asserts → clojure.test).

  EDN fixtures (manifest / ontology schema / lexicons / seed) are repository-local and load
  via fuchi.methods.edn. The independent repository is the actor root; no monorepo-relative
  lookup is permitted.

  The Python `_run` demo printer is omitted (clojure.test provides the runner)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])
            [fuchi.methods.edn :as edn]))

;; ── fixture locators (*file*-relative; repo root + actor root) ────────────────
#?(:clj (def ^:private actor-dir (io/file (System/getProperty "user.dir"))))
#?(:clj (def ^:private schema-file
          (io/file actor-dir "schema" "maintainer-sustenance-ontology.edn")))
#?(:clj (def ^:private manifest-file (io/file actor-dir "manifest.edn")))
#?(:clj (def ^:private seed-file (io/file actor-dir "data" "seed-sustenance-graph.kotoba.edn")))

#?(:clj (defn- manifest-record [] (clojure.edn/read-string (slurp manifest-file))))
#?(:clj (defn- manifest [] (:actor/manifest (manifest-record))))

;; ── tests ─────────────────────────────────────────────────────────────────
(deftest test-manifest-cells-match-cell-dirs
  #?(:clj
     (let [declared (set (map #(get % "name") (get (manifest) "cells")))
           dirs (set (->> (.listFiles (io/file actor-dir "src" "fuchi" "cells"))
                          (filter #(.isDirectory %))
                          (map #(.getName %))
                          (remove #(str/starts-with? % "__"))))]
       (is (= declared dirs) (str "manifest " declared " != dirs " dirs)))))

(deftest test-manifest-lexicons-match-lex-files
  #?(:clj
     (let [declared (set (map #(get % "id") (get (manifest) "lexiconNamespaces")))
           files (set (->> (.listFiles (io/file actor-dir "lex"))
                           (filter #(str/ends-with? (.getName %) ".edn"))
                           (map #(get (edn/load-edn %) ":id"))))]
       (is (every? files declared)
           (str "manifest declares missing lexicons: "
                (remove files declared))))))

(deftest test-manifest-adr-matches-ontology
  #?(:clj
     (let [onto (edn/load-edn schema-file)]
       (is (some #{"2606052300"}
                 (let [adrs (get onto ":ontology/adr")]
                   (if (sequential? adrs) adrs [adrs]))))
       (is (str/ends-with? (get-in (manifest) ["adr" "master"]) "2606052300")))))

(deftest test-manifest-schema-pointer-exists
  #?(:clj
     (let [pointer (get-in (manifest) ["references" "schema"])
           relative (str/replace-first pointer #"^/" "")
           p (io/file actor-dir relative)]
       (is (.exists p) (str p))
       (is (= (.getCanonicalFile schema-file) (.getCanonicalFile p))))))

(deftest test-every-seed-maintainer-has-envelope-or-is-excluded
  #?(:clj
     (let [seed (edn/load-edn seed-file)
           env-dids (set (map #(get % ":envelope/maintainer") (get seed ":envelope/batch")))]
       (doseq [m (get seed ":maintainer/batch")]
         (is (contains? env-dids (get m ":maintainer/did")) (get m ":maintainer/did"))))))

(deftest test-seed-cash-is-zero-everywhere
  #?(:clj
     (let [seed (edn/load-edn seed-file)]
       (doseq [e (get seed ":envelope/batch")]
         (is (= (get e ":envelope/cash-usd-micros") 0))))))

(deftest test-seed-no-maintainer-owns-payoff
  #?(:clj
     (let [seed (edn/load-edn seed-file)]
       (doseq [m (get seed ":maintainer/batch")]
         (is (false? (get m ":maintainer/owns-payoff" false)))))))

(deftest test-seed-covenants-are-valid-vocab
  #?(:clj
     (let [seed (edn/load-edn seed-file)
           vocab (set (get (edn/load-edn schema-file) ":ontology/covenants"))]
       (doseq [m (get seed ":maintainer/batch")]
         (is (contains? vocab (get m ":maintainer/covenant")))))))

(deftest test-seed-envelope-lines-are-valid-vocab
  #?(:clj
     (let [seed (edn/load-edn seed-file)
           vocab (set (get (edn/load-edn schema-file) ":ontology/envelope-lines"))]
       (doseq [e (get seed ":envelope/batch")]
         (is (contains? vocab (get e ":envelope/line")))))))
