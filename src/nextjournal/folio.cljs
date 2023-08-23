(ns nextjournal.folio
  (:require ["@codemirror/commands" :refer [history historyKeymap]]
            ["@codemirror/language" :refer [HighlightStyle syntaxHighlighting LanguageDescription]]
            ["@codemirror/state" :refer [Compartment EditorState RangeSet RangeSetBuilder Text]]
            ["@codemirror/view" :refer [EditorView Decoration keymap placeholder]]
            ["@lezer/highlight" :refer [tags highlightTree]]
            ["react" :as react]
            [applied-science.js-interop :as j]
            [nextjournal.folio.localstorage :as localstorage]
            [nextjournal.clojure-mode :as clojure-mode]
            [nextjournal.clojure-mode.extensions.eval-region :as eval-region]
            [nextjournal.clojure-mode.keymap :as clojure-mode.keymap]
            [nextjournal.command-bar :as command-bar]
            [reagent.core :as reagent]))

(reagent/set-default-compiler! (reagent/create-compiler {:function-components true}))

(def bar-height 26)

(def local-storage-dark-mode-key "darkmode")

(def !dark-mode?
  (reagent/atom (boolean (localstorage/get-item local-storage-dark-mode-key))))

(defn set-dark-mode! [dark-mode?]
  (let [class-list (.-classList (js/document.querySelector "html"))]
    (if dark-mode?
      (.add class-list "dark")
      (.remove class-list "dark")))
  (localstorage/set-item! local-storage-dark-mode-key dark-mode?))

(defn setup-dark-mode! []
  (add-watch !dark-mode? ::dark-mode-watch
             (fn [_ _ old dark-mode?]
               (when (not= old dark-mode?)
                 (set-dark-mode! dark-mode?))))
  (when @!dark-mode?
    (set-dark-mode! @!dark-mode?)))

(def theme (Compartment.))

(defn get-theme []
  (.theme EditorView
          (j/lit {"&.cm-focused" {:outline "none"}
                  ".cm-line" {:padding "0"
                              :line-height "1.6"
                              :font-size "15px"
                              :font-family "\"Fira Mono\", monospace"}
                  ".cm-matchingBracket" {:border-bottom "1px solid var(--teal-color)"
                                         :color "inherit"}

                  ;; only show cursor when focused
                  ".cm-cursor" {:visibility "hidden"}
                  "&.cm-focused .cm-cursor" {:visibility "visible"
                                             :animation "steps(1) cm-blink 1.2s infinite"}
                  "&.cm-focused .cm-selectionBackground" {:background-color "Highlight"}
                  ".cm-tooltip" {:border "1px solid rgba(0,0,0,.1)"
                                 :border-radius "3px"
                                 :overflow "hidden"}
                  ".cm-tooltip > ul > li" {:padding "3px 10px 3px 0 !important"}
                  ".cm-tooltip > ul > li:first-child" {:border-top-left-radius "3px"
                                                       :border-top-right-radius "3px"}
                  ".cm-tooltip.cm-tooltip-autocomplete" {:border "0"
                                                         :border-radius "6px"
                                                         :box-shadow "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                                                         "& > ul" {:font-size "12px"
                                                                   :font-family "'Fira Code', monospace"
                                                                   :background "rgb(241 245 249)"
                                                                   :border "1px solid rgb(203 213 225)"
                                                                   :border-radius "6px"}}
                  ".cm-tooltip-autocomplete ul li[aria-selected]" {:background "rgb(79 70 229)"
                                                                   :color "#fff"}
                  ".cm-tooltip.cm-tooltip-hover" {:background "rgb(241 245 249)"
                                                  :border-radius "6px"
                                                  :border "1px solid rgb(203 213 225)"
                                                  :box-shadow "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)"
                                                  :max-width "550px"}}) #js {:dark @!dark-mode?}))

(defn use-dark-mode [editor-view]
  (react/useEffect
   (fn []
     (add-watch !dark-mode? ::dark-mode #(.dispatch editor-view #js {:effects (.reconfigure theme (get-theme))}))
     #(remove-watch !dark-mode? ::dark-mode))
   #js[]))

(def highlight-style
  (.define HighlightStyle
           (clj->js [{:tag (.-meta tags) :class "cmt-meta"}
                     {:tag (.-link tags) :class "cmt-link"}
                     {:tag (.-heading tags) :class "cmt-heading"}
                     {:tag (.-emphasis tags) :class "cmt-italic"}
                     {:tag (.-strong tags) :class "cmt-strong"}
                     {:tag (.-strikethrough tags) :class "cmt-strikethrough"}
                     {:tag (.-keyword tags) :class "cmt-keyword"}
                     {:tag (.-atom tags) :class "cmt-atom"}
                     {:tag (.-bool tags) :class "cmt-bool"}
                     {:tag (.-url tags) :class "cmt-url"}
                     {:tag (.-contentSeparator tags) :class "cmt-contentSeparator"}
                     {:tag (.-labelName tags) :class "cmt-labelName"}
                     {:tag (.-literal tags) :class "cmt-literal"}
                     {:tag (.-inserted tags) :class "cmt-inserted"}
                     {:tag (.-string tags) :class "cmt-string"}
                     {:tag (.-deleted tags) :class "cmt-deleted"}
                     {:tag (.-regexp tags) :class "cmt-regexp"}
                     {:tag (.-escape tags) :class "cmt-escape"}
                     {:tag (.. tags (special (.-string tags))) :class "cmt-string"}
                     {:tag (.. tags (definition (.-variableName tags))) :class "cmt-variableName"}
                     {:tag (.. tags (local (.-variableName tags))) :class "cmt-variableName"}
                     {:tag (.-typeName tags) :class "cmt-typeName"}
                     {:tag (.-namespace tags) :class "cmt-namespace"}
                     {:tag (.-className tags) :class "cmt-className"}
                     {:tag (.. tags (special (.-variableName tags))) :class "cmt-variableName"}
                     {:tag (.-macroName tags) :class "cmt-macroName"}
                     {:tag (.. tags (definition (.-propertyName tags))) :class "cmt-propertyName"}
                     {:tag (.-comment tags) :class "cmt-comment"}
                     {:tag (.-invalid tags) :class "cmt-invalid"}])))

(defn eval-region* [get-region-fn eval-fn editor-state]
  (when-some [code-str (get-region-fn editor-state)]
    (eval-fn code-str))
  true)

(defn on-change-ext [{:keys [on-change on-change-selection]}]
  (.. EditorState -transactionExtender
      (of (fn [^js tr]
            (when (and on-change (.-docChanged tr)) (on-change tr))
            (when (and on-change-selection (.-selection tr)) (on-change-selection tr))
            #js {}))))

(defn default-extensions [{:as state :keys [on-eval-doc on-eval-expr] placeholder-text :placeholder}]
  #js [clojure-mode/default-extensions
       (syntaxHighlighting highlight-style)
       (.of theme (get-theme))
       (placeholder placeholder-text)
       (history)
       command-bar/extension
       (eval-region/extension {:modifier "Meta"})
       (on-change-ext state)
       (.of keymap historyKeymap)
       (.of keymap clojure-mode.keymap/complete)
       (.of keymap
            (j/lit
             [{:key "Alt-Enter"
               :run (fn eval-doc [editor-view]
                      (on-eval-doc (.. editor-view -state -doc toString)))}
              {:key "Mod-Enter"
               :shift (fn eval-top-level-form [editor-view]
                        (eval-region* eval-region/top-level-string on-eval-expr (.-state editor-view)))
               :run (fn eval-form-at-cursor [editor-view]
                      (eval-region* eval-region/cursor-node-string on-eval-expr (.-state editor-view)))}]))])

(defn make-view [state parent]
  (EditorView. (j/obj :state state :parent parent)))

(defn make-state [{:as state :keys [doc extensions on-change]}]
  (.create EditorState (j/obj :doc doc :extensions (cond-> (default-extensions state)
                                                     (seq extensions) (.concat extensions)))))

(defn display [& content]
  (into
   [:div.border-t.border-slate-300.dark:border-slate-600.px-4.py-2.flex-shrink-0.absolute.left-0.w-screen.bg-white.dark:bg-slate-950
    {:style {:box-shadow "0 -2px 3px 0 rgb(0 0 0 / 0.025)" :bottom bar-height}}]
   content))

(defn split [& content]
  (into [:div.grid {:class (str "grid-cols-" (count (remove nil? content)))}] content))

(js/console.log :react-version (.-version react))

(defn editor [{:as state :keys [doc commands]}]

  (let [editor-el (react/useRef nil)
        [editor-view set-editor-view!] (react/useState nil)]
    (react/useEffect
     (fn []
       (let [editor-view* (make-view (make-state state) (.-current editor-el))]
         (set-editor-view! editor-view*)
         #(.destroy editor-view*)))
     #js[doc])
    (use-dark-mode editor-view)
    [:<>
     [:div.bg-slate-200.border-r.border-slate-300.dark:border-slate-600.px-4.py-3.dark:bg-slate-950.overflow-y-auto.relative
      {:style {:height (str "calc(100vh - " bar-height "px)")}}
      [:div.w-full.h-screen {:ref editor-el}]]
     [:div.fixed.left-0.bottom-0.w-screen {:style {:height bar-height}}
      [command-bar/view (merge {} commands)]]]))
