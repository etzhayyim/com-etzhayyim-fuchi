(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[clojure.test :as t])

(def contracts (edn/read-string (slurp "repository-contracts.edn")))
(doseq [path (:required contracts)]
  (assert (.isFile (io/file path)) (str "required file missing: " path)))
(doseq [file (file-seq (io/file "."))
        :when (and (.isFile file)
                   (str/ends-with? (.getName file) ".edn")
                   (not (str/includes? (.getPath file) "/.git/")))]
  (edn/read-string (slurp file)))

(let [forbidden (set (:forbidden-files contracts))
      bad (->> (file-seq (io/file "."))
               (filter #(.isFile %))
               (remove #(str/includes? (.getPath %) "/.git/"))
               (filter #(or (forbidden (.getName %))
                            (some (fn [ext] (str/ends-with? (.getName %) ext))
                                  (:forbidden-extensions contracts)))))]
  (assert (empty? bad) (str "forbidden artifacts: " (mapv #(.getPath %) bad))))

(def preferred-order
  '[fuchi.cells.test-state-machine fuchi.methods.test-provision fuchi.methods.test-book
    fuchi.methods.test-allocate fuchi.methods.test-analyze fuchi.methods.test-charter-invariants
    fuchi.methods.test-route fuchi.methods.test-lexicons fuchi.methods.test-couple
    fuchi.methods.test-consistency fuchi.methods.test-vote fuchi.methods.test-live-gate
    fuchi.methods.test-public-person fuchi.methods.test-l0-enroll fuchi.methods.test-disclosure-hold
    fuchi.methods.test-rail-mitsuho fuchi.methods.test-rail-hikari fuchi.methods.test-public-surface-report
    fuchi.methods.test-displacement-surface fuchi.methods.test-itonami-bridge
    fuchi.methods.test-mitsuho-receive fuchi.methods.test-hikari-receive
    fuchi.methods.test-mitsuho-produce-plan fuchi.methods.test-hikari-produce-plan
    fuchi.methods.test-care-iyashi-receive fuchi.methods.test-care-iyashi-produce-plan
    fuchi.methods.test-rail-housing-commons fuchi.methods.test-rail-tooling-okaimono
    fuchi.methods.test-rail-compute-murakumo fuchi.methods.test-rail-liquidity-warifu
    fuchi.methods.test-tooling-okaimono-receive fuchi.methods.test-tooling-okaimono-produce-plan
    fuchi.methods.test-compute-murakumo-receive fuchi.methods.test-compute-murakumo-produce-plan
    fuchi.methods.test-housing-commons-receive fuchi.methods.test-housing-commons-produce-plan
    fuchi.methods.test-liquidity-warifu-receive fuchi.methods.test-itonami-surplus-ledger
    fuchi.methods.test-displacement-l0-path fuchi.methods.test-liberation-ladder
    fuchi.methods.test-stage-sustenance fuchi.methods.test-disclosure-continuity
    fuchi.methods.test-displacement-book fuchi.methods.test-displacement-couple
    fuchi.methods.test-displacement-scorecard fuchi.methods.test-displacement-tenure
    fuchi.methods.test-displacement-pipeline fuchi.methods.test-displacement-gov
    fuchi.methods.test-pipeline-audit-ledger fuchi.methods.test-r2-execute
    fuchi.methods.test-pages-deploy fuchi.methods.test-ss-offline-path
    fuchi.methods.test-rail-care-iyashi fuchi.methods.test-pages-publish])

(def test-namespaces
  (->> (file-seq (io/file "test"))
       (filter #(and (.isFile %) (re-find #"test.*\.cljc?$" (.getName %))))
       (map (fn [file]
              (with-open [reader (java.io.PushbackReader. (io/reader file))]
                (second (read reader)))))
       distinct
       (sort-by (zipmap preferred-order (range)))
       vec))
(doseq [namespace test-namespaces] (require namespace))
(let [result (apply t/run-tests test-namespaces)]
  (System/exit (if (zero? (+ (:fail result) (:error result))) 0 1)))
