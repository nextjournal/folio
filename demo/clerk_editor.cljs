(ns clerk-editor
  (:require ["@codemirror/commands" :refer [history historyKeymap]]
            ["@codemirror/state" :refer [EditorState]]
            ["@codemirror/view" :refer [keymap placeholder]]
            ["react-dom/client" :as react-client]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [goog.object :as gobject]
            [nextjournal.clerk.parser :as parser]
            [nextjournal.clerk.render :as render]
            [nextjournal.clerk.render.code :as code]
            [nextjournal.clerk.render.editor :as editor]
            [nextjournal.clerk.render.hooks :as hooks]
            [nextjournal.clerk.render.panel :as panel]
            [nextjournal.clerk.sci-env]
            [nextjournal.clerk.viewer :as v]
            [nextjournal.clojure-mode.extensions.eval-region :as eval-region]
            [nextjournal.clojure-mode.keymap :as clojure-mode.keymap]
            [nextjournal.command-bar :as command-bar]
            [nextjournal.command-bar.keybind :as keybind]
            [nextjournal.folio :as folio]
            [reagent.core :as reagent]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [sci.core :as sci]
            [sci.ctx-store]))

(defn format-1 [fmt-str x]
  (str/replace-first fmt-str "%s" x))

(defn sci-info [{:keys [sym ctx] ns-str :ns}]
  (if-not sym
    {:status ["no-eldoc" "done"]
     :err "Message should contain a `sym`"}
    (let [code (-> "(when-let [the-var (ns-resolve '%s '%s)] (meta the-var))"
                   (format-1 ns-str)
                   (format-1 sym))
          [kind val] (try [::success (sci/eval-string* ctx code)]
                          (catch :default e
                            [::error (str e)]))
          {:keys [doc file line name arglists]} val]
      (if (and name (= kind ::success))
        (cond-> {:ns (some-> val :ns ns-name)
                 :arglists (pr-str arglists)
                 :eldoc (mapv #(mapv str %) arglists)
                 :arglists-str (.join (apply array arglists) "\n")
                 :status ["done"]
                 :name name}
          doc (assoc :doc doc)
          file (assoc :file file)
          line (assoc :line line))
        {:status ["done" "no-eldoc"]}))))

(defn get-block-id [!id->count {:as block :keys [var form type doc]}]
  (let [id->count @!id->count
        id (if var
             var
             (let [hash-fn hash]
               (symbol (str *ns*)
                       (case type
                         :code (str "anon-expr-" (hash-fn form))
                         :markdown (str "markdown-" (hash-fn doc))))))]
    (swap! !id->count update id (fnil inc 0))
    (if (id->count id)
      (symbol (str *ns*) (str (name id) "#" (inc (id->count id))))
      id)))

(defn analyze [form]
  (cond-> {:form form}
    (and (seq? form)
         (str/starts-with? (str (first form)) "def"))
    (assoc :var (second form))))

(defn ns-resolver [notebook-ns]
  (into {} (map (juxt key (comp ns-name val))) '{clerk nextjournal.clerk}))

(defn parse-ns-aliases [ns-form]
  (some (fn [x]
          (when (and (seq? x)
                     (= :require (first x)))
            (into {}
                  (keep (fn [require-form]
                          (when (and (vector? require-form)
                                     (= 3 (count require-form))
                                     (contains? #{:as :as-alias} (second require-form)))
                            ((juxt peek first) require-form))))
                  (rest x))))
        ns-form))

;; TODO: unify with `analyzer/analyze-doc` and move to parser
(defn analyze-doc
  ([doc]
   (analyze-doc {:doc? true} doc))
  ([{:as state :keys [doc?]} doc]
   (binding [*ns* *ns*]
     (let [!id->count (atom {})]
       (cond-> (reduce (fn [{:as state notebook-ns :ns :keys [ns-aliases]} i]
                         (let [{:as block :keys [type text]} (get-in doc [:blocks i])]
                           (if (not= type :code)
                             (assoc-in state [:blocks i :id] (get-block-id !id->count block))
                             (let [node (p/parse-string text)
                                   form (try (n/sexpr node (when ns-aliases {:auto-resolve ns-aliases}))
                                             (catch js/Error e
                                               (throw (ex-info (str "Clerk analysis failed reading block: "
                                                                    (ex-message e))
                                                               {:block block
                                                                :file (:file doc)}
                                                               e))))
                                   analyzed (cond-> (analyze form)
                                              (:file doc) (assoc :file (:file doc)))
                                   block-id (get-block-id !id->count (merge analyzed block))
                                   analyzed (assoc analyzed :id block-id)]
                               (cond-> state
                                 (and (not ns-aliases) (parser/ns? form)) (assoc :ns-aliases (parse-ns-aliases form))
                                 doc? (update-in [:blocks i] merge analyzed)
                                 doc? (assoc-in [:blocks i :text-without-meta]
                                                (parser/text-with-clerk-metadata-removed text (ns-resolver notebook-ns)))
                                 (and doc? (not (contains? state :ns))) (merge (parser/->doc-settings form) {:ns *ns*}))))))
                       (cond-> state
                         doc? (merge doc))
                       (-> doc :blocks count range))
         doc? (-> parser/add-block-settings
                  parser/add-open-graph-metadata
                  parser/filter-code-blocks-without-form))))))

(defn eval-blocks [doc]
  (update doc :blocks (partial map (fn [{:as cell :keys [type text var form]}]
                                     (cond-> cell
                                       (= :code type)
                                       (assoc :result
                                              {:nextjournal/value (cond->> (eval form)
                                                                    var (hash-map :nextjournal.clerk/var-from-def))}))))))

(defn eval-notebook [code]
  (->> code
       (parser/parse-clojure-string {:doc? true})
       (analyze-doc)
       (eval-blocks)
       (v/with-viewer v/notebook-viewer)
       v/present))

(def !eval-result (reagent/atom nil))
(def !notebook (reagent/atom nil))

(defn keys-view [spec]
  (into [:span.inline-flex {:class "gap-[2px]"}]
        (map (fn [k] [:span.rounded-sm.shadow.border.border-slate-300.shadow-inner.font-bold.leading-none.text-center
                     {:class "px-[3px] py-[1px] min-w-[16px]"} k]))
        (str/split (command-bar/get-pretty-spec spec) #" ")))

(defn key-description [{:keys [codemirror? run spec var]}]
  [:div.font-mono {:class "text-[12px]"}
   [:div
    [keys-view spec]
    " is bound to "
    (when codemirror?
      [:span "CodeMirror's "])
    [:span
     (when-let [ns (some-> var meta :ns)]
       [:span ns "/"])
     [:span.font-bold (command-bar/get-fn-name run)]]
    (when codemirror?
      " command")]
   (when-let [docs (some-> var meta :doc)]
     [:div.mt-3 docs])])

(defn doc-view [sym]
  [:div.font-mono {:class "text-[12px]"}
   (let [{:as info :keys [arglists-str doc ns name]} (sci-info {:sym sym :ns "user" :ctx (sci.ctx-store/get-ctx)})]
     (if ns
       [:div
        [:div.flex.gap-2
         [:div.font-bold ns "/" name]
         [:div arglists-str]]
        (let [bindings (keep (fn [{:as binding :keys [var]}]
                               (when-let [{:keys [ns name]} (meta var)]
                                 (when (and (= ns (:ns info)) (= name (:name info)))
                                   binding)))
                             @command-bar/!global-bindings)]
          (when (seq bindings)
            (into [:div.mt-3 "It is bound to "]
                  (map-indexed (fn [i {:keys [spec]}]
                                 [:<>
                                  (when-not (zero? i)
                                    [:span ", and "])
                                  [keys-view spec]]))
                  bindings)))
        (when doc
          [:div.mt-3 doc])]
       [:div "No docs found for " [:span.font-bold sym] "."]))])

(defn doc
  "Shows bindings and doc for a given function."
  []
  (command-bar/toggle-interactive!
   (fn [!state]
     (hooks/use-effect
      (fn []
        (keybind/disable!)
        #(keybind/enable!)))
     [:<>
      [command-bar/label {:text "Which name:"}]
      [command-bar/input !state {:placeholder "Enter name…"
                                 :default-value (or (:doc/name @!state) "")
                                 :on-key-down (fn [event]
                                                (when (= (.-key event) "Enter")
                                                  (swap! !eval-result assoc :result (reagent/as-element [doc-view (:doc/name @!state)])))
                                                (when (contains? #{"Escape" "Enter"} (.-key event))
                                                  (.preventDefault event)
                                                  (.stopPropagation event)
                                                  (command-bar/kill-interactive!)
                                                  (swap! !state dissoc :doc/name)))
                                 :on-input (fn [event]
                                             (swap! !state assoc :doc/name (.. event -target -value)))}]])))

(defn describe-key
  "Describes which function a key or sequence of keys is bound to. Shows the bound function's docstring when available."
  []
  (command-bar/toggle-interactive!
   (fn [!state]
     (hooks/use-effect
      (fn []
        (keybind/disable!)
        #(keybind/enable!)))
     [:<>
      [command-bar/label {:text "Describe key:"}]
      [command-bar/input !state {:placeholder "Press a key or binding…"
                                 :default-value (if-let [spec (:describe-key/spec @!state)]
                                                  (command-bar/get-pretty-spec spec)
                                                  "")
                                 :on-key-down (fn [event]
                                                (.preventDefault event)
                                                (.stopPropagation event)
                                                (if (= (.-key event) "Escape")
                                                  (command-bar/kill-interactive!)
                                                  (let [chord (keybind/e->chord event)
                                                        spec (cond-> chord
                                                               (command-bar/mod-only-chord? chord) (dissoc :button)
                                                               true command-bar/chord->spec)]
                                                    (swap! !state assoc :describe-key/spec spec))))
                                 :on-key-up (fn [event]
                                              (when-let [spec (:describe-key/spec @!state)]
                                                (when-let [binding (command-bar/get-binding-by-spec spec)]
                                                  (swap! !eval-result assoc :result (reagent/as-element [key-description binding])))
                                                (swap! !state dissoc :describe-key/spec)
                                                (command-bar/kill-interactive!)))}]])))

(defn editor []
  (let [!state (hooks/use-state {:commands {:describe-key {:binding "Alt-d" :run describe-key}
                                            :doc {:binding "Shift-Alt-d" :run doc}}
                                 :doc ""
                                 :on-change-selection #(reset! !eval-result nil)
                                 :on-eval-doc #(reset! !notebook (eval-notebook %))
                                 :on-eval-expr #(reset! !eval-result (try {:result (sci/eval-string* (sci.ctx-store/get-ctx) %)}
                                                                          (catch js/Error e {:error (ex-message e)})))})]
    [:<>
     [folio/split
      [folio/editor @!state]
      (when-some [notebook @!notebook]
        [:div
         [:> render/ErrorBoundary {:hash (gensym)}
          [render/inspect-presented notebook]]])]
     (when-let [{:keys [error result]} @!eval-result]
       [folio/display (cond
                        error [:div.text-red-600.text-xs.font-mono error]
                        (render/valid-react-element? result) result
                        :else (render/inspect result))])]))

(defonce react-root
  (react-client/createRoot (js/document.getElementById "root")))

(defn ^:dev/after-load render []
  (when react-root
    (.render react-root (reagent/as-element [editor]))))
