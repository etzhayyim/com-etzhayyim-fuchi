(ns fuchi.methods.displacement-book
  "displacement_book.cljc — offline toritate/kanae projection for displacement SS path.

  Maps stage-sustenance dry packages → route rails → book-toritate ledger entries
  + flow-graph. cash≡0, no payroll/wage. live write default refuse (G10).
  Portable .cljc."
  (:require [fuchi.methods.book :as book]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.r2-execute :as r2]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; stage package kind → provision rail kind
(def KIND->RAIL
  {"food" "food-mitsuho"
   "energy" "energy-hikari"
   "care" "care-iyashi"
   "housing" "housing-commons"
   "tooling" "tooling-okaimono"
   "compute" "compute-murakumo"})

(def KIND->PROVIDER
  {"food" "mitsuho"
   "energy" "hikari"
   "care" "iyashi"
   "housing" "commons-land"
   "tooling" "okaimono"
   "compute" "murakumo"})

(defn stage-packages->rails
  "Convert stage-sustenance :packages map → bookable rail vectors."
  [stage-pkg]
  (vec
   (keep
    (fn [[kind v]]
      (when-let [rail (get KIND->RAIL kind)]
        (let [imp (long (or (:imputed-usd-micros-yr v) 0))]
          (when (pos? imp)
            {:kind rail
             :provider-actor (get KIND->PROVIDER kind kind)
             :imputed-usd-micros-yr imp
             :member-principal false}))))
    (or (:packages stage-pkg) {}))))

(defn book-subject
  "Offline book + flow graph for one displaced subject stage package.
   write-live status is refused by default."
  [subject-did stage-pkg & {:keys [alloc-id]}]
  (let [aid (or alloc-id (str "disp-book-" subject-did))
        rails (stage-packages->rails stage-pkg)
        entries (book/book-toritate rails aid subject-did)
        edges (book/flow-graph rails aid subject-did)
        cats (frequencies (map :category entries))
        write-status (live-gate/gate-status
                      (live-gate/make-live-gate {:leg "book"}) {})
        out {:phase :booked-offline
             :alloc-id aid
             :subject-did subject-did
             :stage (:stage stage-pkg)
             :rails (mapv :kind rails)
             :ledger-entries entries
             :flow-edges edges
             :category-counts cats
             :entry-count (count entries)
             :in-kind-total-usd-micros
             (reduce + 0 (map :imputed-usd-micros-yr entries))
             :write-live-admissible (boolean (get write-status "admissible"))
             :write-live-status write-status
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "offline toritate projection only — write_live refused by default"}]
    (doseq [e entries]
      (when-not (= 0 (:cash-usd-micros e))
        (throw (ex-info "cash≡0" e))))
    (pp/assert-no-public-scores! out)
    out))

(defn book-enrolled-subject
  "Convenience: book from displacement enroll subject map."
  [subject]
  (when-let [sp (:stage-sustenance subject)]
    (book-subject (:subject-did subject) sp
                  :alloc-id (str "disp-" (:cohort-id subject) "-"
                                 (:subject-did subject)))))

(defn public-book-summary
  "Facts-only summary for public surface (no scores)."
  [booked]
  (let [row {:subject-did (:subject-did booked)
             :stage (:stage booked)
             :entry-count (:entry-count booked)
             :categories (:category-counts booked)
             :in-kind-total-usd-micros (:in-kind-total-usd-micros booked)
             :write-live-admissible false
             :cash-usd-micros 0
             :live false
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (pp/assert-no-public-scores! row)
    row))
