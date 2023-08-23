(ns scittle.build
  (:require-macros [nextjournal.clerk.render.macros :refer [sci-copy-nss]])
  (:require [reagent.core]                                  ;; needs to go first
            [applied-science.js-interop :as j]
            [sci.core :as sci]
            [sci.async :as scia]
            [scittle.core :as scit]
            [scittle.impl.error :as error]
            [shadow.esm :refer [dynamic-import]]
            [sci.configs.applied-science.js-interop :as sci.configs.js-interop]
            [reagent.core]

            ;; FIXME: folio runtime dependencies, what to do with these
            [nextjournal.command-bar]
            [nextjournal.folio.localstorage]
            [nextjournal.clojure-mode]
            [nextjournal.clojure-mode.extensions.eval-region]
            [nextjournal.clojure-mode.keymap]))

(swap! scit/!sci-ctx
       sci/merge-opts {:namespaces (merge
                                    sci.configs.js-interop/namespaces
                                    (sci-copy-nss
                                     'reagent.core
                                     'nextjournal.clojure-mode
                                     'nextjournal.clojure-mode.extensions.eval-region
                                     'nextjournal.clojure-mode.keymap
                                     'nextjournal.command-bar
                                     'nextjournal.folio.localstorage))

                       :async-load-fn (fn [{:keys [ctx libname opts ns]}]
                                        (if (string? libname)
                                          (.then (dynamic-import libname)
                                                 (fn [mod]
                                                   (let [{:keys [as refer]} opts]
                                                     (cond
                                                       as (do
                                                            (sci/add-class! ctx libname mod)
                                                            (sci/add-import! ctx ns libname as))
                                                       refer (doseq [sym refer]
                                                               (let [sub-mod (j/get mod (str sym))
                                                                     sub-name (symbol (str libname "$" sym))]
                                                                 (sci/add-class! ctx sub-name sub-mod)
                                                                 (sci/add-import! ctx ns sub-name sym)))
                                                       :else (js/console.warn
                                                              (str "Import of '" libname "' requires :as or :refer terms.")))
                                                     {:handled true})))
                                          (do (js/console.error :missing-ns libname)
                                              {:handled false})))})

(defn eval-string-async [txt]
  (.. (scia/eval-string* @scit/!sci-ctx txt)
      (catch (fn [error]
               (js/console.error error)
               (error/error-handler error (:src @scit/!sci-ctx))
               (throw error)))))

(defn eval-promise [tag]
  (if-some [src (.getAttribute tag "src")]
    (.. (js/fetch src)
        (then (fn [resp]
                (.then (.text resp)
                       (fn [code-str]
                         (swap! scit/!sci-ctx assoc-in [:src src] code-str)
                         (eval-string-async code-str)))))
        (catch (fn [err] (throw (js/Error "failed to fetch source" err)))))
    (if-some [code-str (.-innerText tag)]
      (do (swap! scit/!sci-ctx assoc-in [:src (str (gensym "scit-tag-"))] code-str)
          (eval-string-async code-str))
      (throw (js/Error "Cant't get tag source")))))

(defn eval-script-tags [{:keys [p tags]}]
  (.. p
      (then (fn [res]
              (js/console.log :res res)
              (if-some [t (first tags)]
                (eval-script-tags {:tags (rest tags) :p (eval-promise t)})
                (js/console.log :done))))
      (catch (fn [err] (js/console.error :halting err)))))

;; this ends up rebuilding the whole scit
(set! scit/eval-script-tags
      (fn []
        (let [script-tags (js/document.querySelectorAll "script[type='application/x-scittle']")]
          (eval-script-tags {:tags script-tags
                             :p (js/Promise.resolve :init)}))))
