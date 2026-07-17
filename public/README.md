# fuchi public surface (static)

Generated offline. cash≡0. live=false. No personal scores.
Priority: wellbecoming > mago > ko > present.
Includes displacement→L0 offline enroll + L6 tenure + audit.

## Deploy membrane (plan-only)

- Default: refused (`FUCHI_ALLOW_PAGES_DEPLOY` unset).
- Gated plan: flag=1 + operator-did → phase=:gated-deploy-plan, still `deployed=false` (no wrangler/API here).
- Actual `wrangler pages deploy public/` is **operator out-of-band**.
- Do not enable live sustenance disbursement from this package.
- land-grant-executed stays 0 until Council-gated live path.

### Operator runbook steps

1. write-deploy-package! → refresh public/ static facts
1. review index.html / facts.edn (no personal scores)
1. optional gated plan: FUCHI_ALLOW_PAGES_DEPLOY=1 + operator-did → phase=:gated-deploy-plan still deployed=false
1. out-of-band: wrangler pages deploy public/
1. never enable live sustenance disbursement from this package

## Deploy status

- phase: :refused
- authorized-to-deploy: false
- package-ready: true
- wrangler-invoked: false
- cloudflare-api-invoked: false
- deployed: false
- live disbursement: never from this package
- last-run land-grant-executed: 0
