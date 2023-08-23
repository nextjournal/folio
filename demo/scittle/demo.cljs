(ns scittle.demo
  (:require ["react-dom/client" :as react-client]
            [reagent.core :as reagent]
            [nextjournal.folio :as folio]))

(defonce react-root
  (react-client/createRoot (js/document.getElementById "root")))

#_(js/document.querySelector "script[type='application/x-folio']")

(defn editor [_]
  [:div
   [:h1.my-20.text-amber-500 "Skittle Bum!"]
   ;; TODO: fix react mismatch
   #_ [folio/editor {:doc "(ns my-ns)"}]])

(.render react-root (reagent/as-element [editor {}]))
