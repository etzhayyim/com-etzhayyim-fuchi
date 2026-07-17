(ns fuchi.methods.social-test
  (:require [clojure.test :refer [deftest is testing]]
            [fuchi.methods.social :as social]))

(deftest dry-run-publication-is-non-adjudicating
  (let [post (social/draft-observation-post
              "maintainer cohort"
              "Two independent observations agree."
              ["did:example:source-a" "did:example:source-b"]
              "did:web:etzhayyim.com:actor:fuchi")]
    (is (= ":dry-run" (get post ":post/status")))
    (is (true? (get post ":post/is-mirror")))
    (is (true? (get post ":post/non-adjudicating-notice")))
    (is (false? (get post ":post/server-held-key")))
    (is (= 2 (count (get post ":post/sources"))))))

(deftest publication-gates-source-provenance-and-live-mode
  (testing "two nonblank sources are required"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"needs ≥ 2 citations"
                          (social/draft-observation-post "subject" "body" ["one"]))))
  (testing "live publication remains refused at R0"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Only dry-run posts"
                          (social/build-live {})))))
