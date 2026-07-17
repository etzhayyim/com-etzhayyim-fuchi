#!/usr/bin/env bash
# fuchi 扶持 — standalone bb/clj suite; no monorepo-relative classpath.
set -euo pipefail
cd "$(dirname "$0")"
exec bb -e '
(require (quote clojure.test)
         (quote fuchi.murakumo-test)
         (quote fuchi.cells.test-state-machine)
         (quote fuchi.methods.test-provision)
         (quote fuchi.methods.test-book)
         (quote fuchi.methods.test-allocate)
         (quote fuchi.methods.test-analyze)
         (quote fuchi.methods.test-public-person)
         (quote fuchi.methods.test-charter-invariants)
         (quote fuchi.methods.test-route)
         (quote fuchi.methods.test-lexicons)
         (quote fuchi.methods.test-couple)
         (quote fuchi.methods.consistency-test)
         (quote fuchi.methods.test-vote)
         (quote fuchi.methods.test-live-gate)
         (quote fuchi.methods.social-test))
(let [namespaces [(quote fuchi.murakumo-test)
                  (quote fuchi.cells.test-state-machine)
                  (quote fuchi.methods.test-provision)
                  (quote fuchi.methods.test-book)
                  (quote fuchi.methods.test-allocate)
                  (quote fuchi.methods.test-analyze)
                  (quote fuchi.methods.test-public-person)
                  (quote fuchi.methods.test-charter-invariants)
                  (quote fuchi.methods.test-route)
                  (quote fuchi.methods.test-lexicons)
                  (quote fuchi.methods.test-couple)
                  (quote fuchi.methods.consistency-test)
                  (quote fuchi.methods.test-vote)
                  (quote fuchi.methods.test-live-gate)
                  (quote fuchi.methods.social-test)]
      result (apply clojure.test/run-tests namespaces)]
  (System/exit (if (zero? (+ (:fail result) (:error result))) 0 1)))'
