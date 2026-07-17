(ns fuchi.methods.pages-publish
  "pages_publish.cljc — offline static site package for public SS facts (Pages-ready).

  Writes public/index.html (+ optional assets) from public_surface_report.
  Does NOT deploy, does NOT call Cloudflare API, live=false.
  Portable .cljc; file I/O at #?(:clj) edge."
  (:require [fuchi.methods.public-surface-report :as rep]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.displacement-scorecard :as dsc]
            #?(:clj [fuchi.methods.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

#?(:clj
   (defn write-pages!
     "Generate Pages-ready static files under public/ (and out/ mirror).
     Includes displacement L4 scorecard snapshot. Never deploys."
     ([]
      (let [actor (or (System/getenv "FUCHI_ACTOR_DIR")
                      (-> *file* io/file .getParentFile .getParentFile .getCanonicalPath))
            seed (edn/load-edn (io/file actor "data" "seed-sustenance-graph.kotoba.edn"))
            html (rep/report-html seed :include-l0-demo true :include-itonami true
                                  :include-scorecard true)
            edn-body (rep/report-edn seed :include-l0-demo true :include-itonami true
                                     :include-scorecard true)
            scard (get edn-body :report/displacement-scorecard {})
            pub (io/file actor "public")
            outd (io/file actor "out")]
        (.mkdirs pub)
        (.mkdirs outd)
        ;; assert no scores in body
        (doseq [f (:report/public-persons edn-body)]
          (pp/assert-no-public-scores! f))
        (spit (io/file pub "index.html") html)
        (spit (io/file pub "facts.edn") (pr-str edn-body))
        (when (seq scard)
          (spit (io/file pub "scorecard.md") (dsc/scorecard-md scard))
          (spit (io/file pub "scorecard.edn") (pr-str scard))
          (spit (io/file outd "displacement-scorecard.md") (dsc/scorecard-md scard))
          (spit (io/file outd "displacement-scorecard.edn") (pr-str scard)))
        (spit (io/file pub "_headers")
              (str "/*\n  X-Frame-Options: DENY\n  X-Content-Type-Options: nosniff\n"
                   "  Referrer-Policy: no-referrer\n"
                   "  Content-Security-Policy: default-src 'self'; style-src 'unsafe-inline'\n"))
        (spit (io/file pub "robots.txt") "User-agent: *\nAllow: /\n")
        (spit (io/file pub "README.md")
              (str "# fuchi public surface (static)\n\n"
                   "Generated offline. cash≡0. live=false. No personal scores.\n"
                   "Priority: wellbecoming > mago > ko > present.\n"
                   "Includes displacement L0→L4 multi-gen path + scorecard snapshot.\n"
                   "Deploy: point Cloudflare Pages (or any static host) at this directory.\n"
                   "Do not enable live disbursement from this package.\n"))
        (spit (io/file outd "public-surface.html") html)
        {:index (str (io/file pub "index.html"))
         :facts (str (io/file pub "facts.edn"))
         :scorecard (when (seq scard) (str (io/file pub "scorecard.md")))
         :live false
         :cash-usd-micros 0
         :score-surface []
         :priority-stack PRIORITY-STACK
         :all-live-refused (boolean (:scorecard/all-live-refused scard))
         :deployed false}))))
