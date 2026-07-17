(ns fuchi.methods.social
  "Actor-specific adapter over the shared social publication membrane."
  (:require [etzhayyim.social.publication :as publication]))

(def config
  {:actor-id "fuchi"
   :display-name "扶持 — Maintainer Sustenance Allocator (investment-fund inverse)"})

(def DISCLAIMER (publication/disclaimer config))

(defn draft-observation-post
  ([subject body sources]
   (publication/draft-observation-post config subject body sources))
  ([subject body sources author]
   (publication/draft-observation-post config subject body sources author)))

(defn build-live [& args]
  (apply publication/build-live config args))
