(ns spinner.core
  (:require [clojure.java.io   :as io]
            [clojure.string    :as str]
            [clojure-csv.core  :as csv])
  (:import java.util.regex.Pattern java.io.File))

(def spinners (atom {}))
(def ^:dynamic *data* nil)
(def ^:dynamic spinner-ns nil)


(def hash-regex #"\"|\\\{|\\|(?<!\\)\{[^{}]+\}")

(defn hash-clojurizer [^String match]
  (cond
    (= match "\"")
    "\\\""
    (= match "\\")
    "\\\\"
    (= match "\\{")
    "{"

    (= \{ (first match))
    (let [expr (subs match 1 (dec (count match)))
          expr (if (.contains match " ") (str \( expr \)) #_else expr)]
      (str "\" " expr "\"")) ; TODO: Allow to be namespace independent

    :else match))

(defn parse-str [string]
  (str "[\"" (clojure.string/replace string hash-regex hash-clojurizer) "\"]"))

(defn- resolve-symbolé [s]
  (cond
    (or (string? s) (keyword? s))
      s
    (or (list? s) (seq? s))
      (mapv resolve-symbolé s)
    :otherwise
      (if-let [spin-fn (resolve s)] spin-fn
        (throw (IllegalArgumentException. (str "Var not found: {" s "} in namespace: " *ns* ))))))

(defn compile-str
  "Compiles a single string into a literal string or a seq of vars + strings.
  See also: apply-str"
  [string]
  (let [[head & tail :as parsed]
        (->>  (parse-str string)
              (read-string)
              (remove #(= "" %)))]
    (if (and (empty? tail) (string? head))
      head
    ;else
      (resolve-symbolé parsed))))

(defn eval-compiled-str [%]
  (cond (string?  %) %
        (keyword? %) (% *data*)
        (vector?  %) (apply (first %) (map eval-compiled-str (next %)))
        :else (%)))

(defn apply-str-transform
  "Takes a parsed and compiled string/seq and transforms it into a seq of strings."
  [result]
  (if (string? result)
    result
    (map eval-compiled-str result)))

(defn apply-str
  "Takes a parsed and compiled string/seq and transforms it into a regular string."
  [result]
  (if (string? result)
    result
    (apply str (map eval-compiled-str result))))

(defn- fn-expl [fn] (re-matches #"(.+?)(?:\.([^.]*$))?" (str fn)))
(defn- extension [fn]
  (last (fn-expl fn)))

(defn- basename [fn]
  (second (fn-expl fn)))

; Multi method
(defmulti keyword->fnc (fn [spinner-name values fnc]
  (type values)))

(declare compile-vector)

(defn compile-conditional-column [vector-map key key-prefix pre input]
  ;(prn key key-prefix pre input)
  (case pre
    "0"
      `(let [v# ~input] (or (nil? v#) (= ~(Integer/parseInt pre) v#)))
    ("1" "2" "3" "4" "5" "6" "7" "8" "9")
      `(= ~(Integer/parseInt pre) ~input)
    "n"
      (if-not (get vector-map (str ":" key-prefix ":0"))
        `(when-let [v# ~input] (or (= v# 0) (> v# 1)))
      ;else there's a 0-th column available
        `(some-> ~input (> 1)))
    (read-string pre)))

(defn compile-keyed-vectors-cases [values]
  (for [[k vs] values :when (not (empty? k))
        :let [[_ kw n] (.split (str k) ":" 3)]]
    [ (let [r (compile-conditional-column values k kw
                (if (-> kw first str (= "(")) (subs k 1) #_else n)
                (list (keyword (symbol kw)) `*data*))]
        #_(prn r) r)
      (compile-vector vs) ]))

(defn compile-expression-keyed-vectors [values]
  `(cond ;values are: word lists, keyed by expression
    ~@(apply concat (compile-keyed-vectors-cases values))
    ~@(let [r (some->> (get values "") (compile-vector) (list :else))] #_(prn r) r)))

(defn compile-vector
  "Takes a seq of string and returns a vector of compiled strings usable by apply-str.
  See also: compile-str"
  [values]
  (if (-> values meta :compiled)
    values
  ;else
    (if (map? values)
      (eval (list `fn ['i]
              (list `when-let ['v (compile-expression-keyed-vectors values)] (list 'v 'i))))
    ;else just a word list
      (with-meta (mapv compile-str values) {:compiled true}))))

; Vector
(defmethod keyword->fnc clojure.lang.PersistentVector [spinner-name values fnc]
  (let [values         (compile-vector values)
        spinner-values (intern spinner-ns (symbol (str spinner-name "*")) values)]
    #(apply-str (fnc (var-get spinner-values)))))

; Map
(defn with-key-colon-meta [[k vs]]
  (let [[group] (.split (str k) ":" 2)]
    (with-meta vs {:group group :key (subs k (count group))})))

(defn group-by-colon [spin-columns-map]
  ;(prn "group-by-colon" spin-columns-map)
  (->> spin-columns-map
    (map with-key-colon-meta)
    (group-by #(-> % meta :group))
    (map (fn [[k vs]]
           [k (if-not (next vs) (first vs)
               #_else (zipmap (map #(-> % meta :key) vs) vs))]))
    (into {})))

(defmethod keyword->fnc clojure.lang.IPersistentMap [spinner-name values fnc]
  (assert (not (string? values)))
  (let [[spin s-keys] (first values)
        s-keys        (compile-vector s-keys)
        spin-fn       (intern spinner-ns (symbol spin))
        spin-dynamic  (intern spinner-ns (symbol (str "*" spin "*")) nil)
        spin-indices  (zipmap s-keys (range))
        spin-columns  (group-by-colon (into {} (rest values)))]
    (println "Handle spinner-name: '" spinner-name "' with keys: " (keys values))
    (.setDynamic ^clojure.lang.Var spin-dynamic true)
    (alter-meta! spin-dynamic assoc :dynamic true)
    (doseq [[col-name col-vals] spin-columns
            :let [col-name (str/trim col-name)] :when (not (empty? col-name))]
      (println (intern spinner-ns (symbol (str spin "->" col-name))
      (let [compiled-vals (compile-vector col-vals)]
        (fn colval
          ([arg]
            (apply-str (some-> arg spin-indices compiled-vals)))
          ([]
            (colval (or (var-get spin-dynamic) (fnc s-keys)))))))))
    (keyword->fnc spinner-name s-keys (fn [%] (if-let [dyn (var-get spin-dynamic)] dyn (fnc %))))))

; File
(defmulti from-file (fn [spinner-name file arrow-vars-only? fnc]
  (extension file)))

(defmethod from-file :default [spinner-name file arrow-vars-only? fnc]
  (println "Spinning " file)
  (let [content
    (with-open [csv-file (io/reader file)]
      (let [data              (csv/parse-csv csv-file)
            [columns & data]  (if arrow-vars-only? (take 1 data) (doall data))
            columns           (cons (str spinner-name) (rest columns))]
        (loop [c columns i 0 result (transient [])]
          (if-not (first c)
            (apply array-map (persistent! result))
          ;else
            (let [result (conj! result (first c))
                  result (conj! result (into [] (map #(nth % i nil) data)))]
              (recur (next c) (inc i) result))))))]
  (keyword->fnc spinner-name content fnc)))

(defmethod keyword->fnc java.io.File [spinner-name values fnc]
  (from-file spinner-name values false fnc))

; :default
(defmethod keyword->fnc :default [spinner-name values fnc]
  (println "Could not handle spinner-name: '" spinner-name "' with values: " values) values)

(defn- create-spinner-ns [spinner-name]
  (cond
    (and (symbol? spinner-name) (namespace spinner-name))
    (let [ns-sym (symbol (namespace spinner-name))]
      (if-let [var-ns (find-ns ns-sym)] var-ns #_else (create-ns ns-sym)))

    (and (symbol? spinner-ns) (find-ns spinner-ns))
    (find-ns spinner-ns)

    (and (symbol? spinner-ns) (nil? (find-ns spinner-ns)))
    (create-ns spinner-ns)

    (not (nil? spinner-ns))
    spinner-ns

    :else ; No spinner-ns set, use default
    (create-ns 'spinner)))

(defn defspin* [spinner-name values fnc]
  (let [spinner-ns
        (create-spinner-ns spinner-name)
        spinner-name
        (if-not (namespace spinner-name) spinner-name (symbol (name spinner-name)))
        spin
        (binding [spinner-ns spinner-ns *ns* spinner-ns]
          (intern spinner-ns spinner-name
            (if (or (nil? values) (fn? values))
              values
            ;else
              (keyword->fnc spinner-name values fnc))))]
    (swap! spinners assoc (str spinner-name) spin)
    [spin values]))

(defmacro defspin
  ([spinner-name values]
  `(binding [spinner-ns (or spinner-ns *ns*)]
     (defspin* '~spinner-name ~values rand-nth)))
  ([spinner-name values fnc]
  `(binding [spinner-ns (or spinner-ns *ns*)]
     (defspin* '~spinner-name ~values ~fnc))))

(defn- spin-args
  "Process 2-arity function arguments"
  [file-or-name fnc-or-file action]
  (if (symbol? file-or-name) ; file-or-name is a name, this fnc-or-file is a file
    (action file-or-name fnc-or-file rand-nth) ; TODO: Make rand-nth configurable through dynamic binding
    (action (symbol (basename file-or-name)) file-or-name fnc-or-file)))


(defn spinfile
  ([file] (spinfile file rand-nth))
  ([file-or-name fnc-or-file]
    (spin-args file-or-name fnc-or-file defspin*))
  ([spinner-name file fnc]
    (defspin* spinner-name file fnc)))

(defn- walk-dir
  [root-ns ^File dir visitor-fn]
  (binding [spinner-ns (or root-ns spinner-ns)]
    (->> (for [^File file (.listFiles dir) :when (not (.isHidden file))]
           (if (.isFile file)
             [(visitor-fn (symbol (basename (.getName file))) file)]
           ;else
             (let [subsym (symbol (str spinner-ns "." (.getName file)))
                   subdir (walk-dir subsym file visitor-fn)]
               ;; Create aliases for all sub-directories
               (doseq [[spinvar _] subdir]
                 (binding [*ns* (or (find-ns spinner-ns) (create-ns spinner-ns))]
                   (let [sym (symbol (subs (name (.. ^clojure.lang.Var spinvar ns name))
                                           (inc (count (name spinner-ns)))))]
                     (when-not (.lookupAlias *ns* sym)
                       (alias sym (.. ^clojure.lang.Var spinvar ns name))))))
               subdir)))
       (doall)
       (apply concat))))

(defn spindir
  "Recursively create spinners for all the files in the given directory
   Sub-directories will create sub-namespaces."
  ([dir] (spindir dir rand-nth))
  ([dir-or-name dir-or-fnc]
    (spin-args dir-or-name dir-or-fnc spindir))
  ([root-ns dir fnc]
    (let [^File dir (io/as-file dir)]
      (assert (.isDirectory dir) (str dir " is not a directory."))
      ;; 1st pass:
      ;;  Declare vars and create namespace aliases
      (dorun (walk-dir root-ns dir
                       (fn [sym ^File file]
                        (let [var+file (defspin* sym nil nil)]
                          ;(println "Creating in " spinner-ns " spin-file var: " sym ", root = " root-ns)
                          (binding [*ns* (find-ns spinner-ns)] (refer root-ns))
                          (from-file sym file true nil)
                          var+file))))
      ;; 2nd pass:
      ;;  Process the files and define the actual Vars
      (into (array-map)
            (walk-dir root-ns dir
                      (fn [sym ^File file] (defspin* sym file fnc)))))))

(defn eval-spin [spinstr]
  (-> spinstr compile-str apply-str))

(defn re-find-ns-names
  "Match the result of (all-ns) against the regex 're"
  [re]
  (->> (all-ns) (map #(re-matches re (str %))) (remove nil?)))

(defn ns-publics-grouped
  "Group all direct sub-namespaces of root-ns by name and apply transform to each.
  Returns a map of the transform results keyed by the namespace name (string after root-ns dot).
  'transform takes the result of (ns-publics sub-ns).
  Example:
    (ns-publics-grouped 'words (fn [publics] (mapv publics ['*type*])))
    => {\"en\" [#'words.en/*type*], \"nl\" [#'words.nl/*type*]}
  "
  [root-ns transform]
  (->>
    (re-pattern (str (name root-ns) "\\.([^.]+)"))
    (re-find-ns-names)
    (map (fn [[ns key :as pair]] [key (transform (ns-publics (symbol ns)))]))
    (into {})))
