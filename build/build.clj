(ns build
  (:require [babashka.fs :as fs]
            [shadow.cljs.cli]
            [shadow.cljs.devtools.api :as shadow.api]
            [babashka.process :refer [sh]]
            [nextjournal.cas-client :as cas-client]))

(defn static-app [_]
  (sh {:out :inherit} "yarn install"))

(comment
  (do (shadow.api/release! :scittle {:debug true}) nil)

  (fs/list-dir "release/js")
  (fs/delete-tree "release")
  (do
    (fs/copy "scittle/index.html" "release/index.html" {:replace-existing true})
    (cas-client/put {:path "release"
                     :namespace "nextjournal"
                     :tag "folio-scittle"
                     :auth-token (System/getenv "CAS_AUTH_TOKEN")})))
