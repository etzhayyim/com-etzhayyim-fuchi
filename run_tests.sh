#!/usr/bin/env bash
# fuchi 扶持 — bb/clj test suite (ADR-2606160842 py→clj; ADR-2607177000 public-person).
set -euo pipefail
ACTOR_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$ACTOR_DIR"

# ns is fuchi.methods.* — provide fuchi/methods via self-symlink (idempotent).
if [[ ! -e fuchi ]]; then
  ln -sfn . fuchi
fi

# etzhayyim/root holds 00-contracts/schemas (maintainer-sustenance-ontology).
if [[ -d "$ACTOR_DIR/../root/00-contracts/schemas" ]]; then
  export FUCHI_REPO_ROOT="$(cd "$ACTOR_DIR/../root" && pwd)"
elif [[ -d "$ACTOR_DIR/../../etzhayyim/root/00-contracts/schemas" ]]; then
  export FUCHI_REPO_ROOT="$(cd "$ACTOR_DIR/../../etzhayyim/root" && pwd)"
fi
export FUCHI_ACTOR_DIR="$ACTOR_DIR"

exec bb -cp "$ACTOR_DIR" -e '
(require
  (quote clojure.test)
  (quote fuchi.cells.test-state-machine)
  (quote fuchi.methods.test-provision)
  (quote fuchi.methods.test-book)
  (quote fuchi.methods.test-allocate)
  (quote fuchi.methods.test-analyze)
  (quote fuchi.methods.test-charter-invariants)
  (quote fuchi.methods.test-route)
  (quote fuchi.methods.test-lexicons)
  (quote fuchi.methods.test-couple)
  (quote fuchi.methods.test-consistency)
  (quote fuchi.methods.test-vote)
  (quote fuchi.methods.test-live-gate)
  (quote fuchi.methods.test-public-person)
  (quote fuchi.methods.test-l0-enroll)
  (quote fuchi.methods.test-disclosure-hold)
  (quote fuchi.methods.test-rail-mitsuho)
  (quote fuchi.methods.test-rail-hikari)
  (quote fuchi.methods.test-public-surface-report)
  (quote fuchi.methods.test-displacement-surface)
  (quote fuchi.methods.test-itonami-bridge)
  (quote fuchi.methods.test-mitsuho-receive)
  (quote fuchi.methods.test-hikari-receive)
  (quote fuchi.methods.test-mitsuho-produce-plan)
  (quote fuchi.methods.test-hikari-produce-plan)
  (quote fuchi.methods.test-care-iyashi-receive)
  (quote fuchi.methods.test-care-iyashi-produce-plan)
  (quote fuchi.methods.test-rail-housing-commons)
  (quote fuchi.methods.test-rail-tooling-okaimono)
  (quote fuchi.methods.test-rail-compute-murakumo)
  (quote fuchi.methods.test-ss-offline-path)
  (quote fuchi.methods.test-rail-care-iyashi)
  (quote fuchi.methods.test-pages-publish))
(let [r (clojure.test/run-tests
          (quote fuchi.cells.test-state-machine)
          (quote fuchi.methods.test-provision)
          (quote fuchi.methods.test-book)
          (quote fuchi.methods.test-allocate)
          (quote fuchi.methods.test-analyze)
          (quote fuchi.methods.test-charter-invariants)
          (quote fuchi.methods.test-route)
          (quote fuchi.methods.test-lexicons)
          (quote fuchi.methods.test-couple)
          (quote fuchi.methods.test-consistency)
          (quote fuchi.methods.test-vote)
          (quote fuchi.methods.test-live-gate)
          (quote fuchi.methods.test-public-person)
          (quote fuchi.methods.test-l0-enroll)
          (quote fuchi.methods.test-disclosure-hold)
          (quote fuchi.methods.test-rail-mitsuho)
          (quote fuchi.methods.test-rail-hikari)
          (quote fuchi.methods.test-public-surface-report)
          (quote fuchi.methods.test-displacement-surface)
          (quote fuchi.methods.test-itonami-bridge)
          (quote fuchi.methods.test-mitsuho-receive)
          (quote fuchi.methods.test-hikari-receive)
          (quote fuchi.methods.test-mitsuho-produce-plan)
          (quote fuchi.methods.test-hikari-produce-plan)
          (quote fuchi.methods.test-care-iyashi-receive)
          (quote fuchi.methods.test-care-iyashi-produce-plan)
          (quote fuchi.methods.test-rail-housing-commons)
          (quote fuchi.methods.test-rail-tooling-okaimono)
          (quote fuchi.methods.test-rail-compute-murakumo)
          (quote fuchi.methods.test-ss-offline-path)
          (quote fuchi.methods.test-rail-care-iyashi)
          (quote fuchi.methods.test-pages-publish))]
  (System/exit (if (zero? (+ (:fail r) (:error r))) 0 1)))
'
