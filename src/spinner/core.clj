(ns spinner.core
  (:require [clojure.java.io   :as io]
            [clojure-csv.core  :as csv])
  (:import java.util.regex.Pattern java.io.File))

(def spinners (atom {}))
(def ^:dynamic spinner-ns nil)


(def hash-regex #"\"|\\\{|\\|(?<!\\)\{[^{}\s]+\}")

(defn hash-clojurizer [match]
  (cond
    (= match "\"")
    "\\\""
    (= match "\\")
    "\\\\"
    (= match "\\{")
    "{"

    (= \{ (first match))
    (str "\" " (subs match 1 (dec (count match))) "\"") ; TODO: Allow to be namespace independent

    :else match))

(defn parse-str [string]
  (str "[\"" (clojure.string/replace string hash-regex hash-clojurizer) "\"]"))

(defn- resolve-symbolé [s]
  (if (string? s)
    s
  ;else
    (doall
      (for [% s]
        (if (string? %) %
        ;else
          (if-let [fn (resolve %)] fn
            (throw (IllegalArgumentException. (str "Var not found: {" % "} in namespace: " *ns* )))))))))

(defn compile-str [string]
  (let [[head & tail :as parsed]
        (->>  (parse-str string)
              (read-string)
              (remove #(= "" %)))]
    (if (and (nil? tail) (string? (first parsed)))
      head
    ;else
      (resolve-symbolé parsed))))

(defn apply-str [result]
  (if (string? result)
    result
    (apply str (map #(if (string? %) % (%)) result))))

(defn- fn-expl [fn] (re-matches #"(.+?)(?:\.([^.]*$))?" (str fn)))
(defn- extension [fn]
  (last (fn-expl fn)))

(defn- basename [fn]
  (second (fn-expl fn)))

; Multi method
(defmulti keyword->fnc (fn [spinner-name values fnc]
  (type values)))

; Vector
(defmethod keyword->fnc clojure.lang.PersistentVector [spinner-name values fnc]
  (let [values         (mapv compile-str values)
        spinner-values (intern spinner-ns (symbol (str spinner-name "*")) values)]
    #(apply-str (fnc (var-get spinner-values)))))

; Map
(defmethod keyword->fnc clojure.lang.PersistentArrayMap [spinner-name values fnc]
  (let [[spin s-keys] (first values)
        spin-fn       (intern spinner-ns (symbol spin))
        spin-dynamic  (intern spinner-ns (symbol (str "*" spin "*")) nil)]
    (.setDynamic ^clojure.lang.Var spin-dynamic true)
    (alter-meta! spin-dynamic assoc :dynamic true)
    (doseq [[kolom-naam kolom-values] (rest values)]
      (intern spinner-ns (symbol (str spin "->" kolom-naam))
      (let [kolom-values (zipmap s-keys (map compile-str kolom-values))]
        #(apply-str (kolom-values (or (var-get spin-dynamic) (spin-fn)))))))
  (keyword->fnc spinner-name s-keys (fn [%] (if-let [dyn (var-get spin-dynamic)] dyn (fnc %))))))

; File
(defmulti from-file (fn [spinner-name file fnc]
  (extension file)))

(defmethod from-file :default [spinner-name file fnc]
  (let [content
    (with-open [csv-file (io/reader file)]
      (let [[columns & data]  (->> (csv/parse-csv csv-file) doall)
            columns           (cons (str spinner-name) (rest columns))]
        (loop [c columns i 0 result (transient (array-map))]
          (if-not (first c)
            (persistent! result)
            (let [result (assoc! result (first c) (into [] (map #(nth % i) data)))]
              (recur (next c) (inc i) result))))))]
  (keyword->fnc spinner-name content fnc)))

(defmethod keyword->fnc java.io.File [spinner-name values fnc]
  (from-file spinner-name values fnc))

; :default
(defmethod keyword->fnc :default [spinner-name values fnc]
  (prn "Could not handle spinner-name" spinner-name) values)

(defn defspin* [spinner-name values fnc]
  (binding [spinner-ns
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

             ;else: No spinner-ns set
             :else
             (create-ns 'spinner))
            *ns* spinner-ns]
    (let [spinner-name
          (if-not (namespace spinner-name) spinner-name (symbol (name spinner-name)))
          spin
          (intern spinner-ns spinner-name
            (if (or (nil? values) (fn? values))
              values
            ;else
              (keyword->fnc spinner-name values fnc)))]
      (swap! spinners assoc (str spinner-name) spin)
      [spin values])))

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
    (->> (for [^File file (.listFiles dir)]
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
                       (fn [sym ^File file] (defspin* sym nil nil))))
      ;; 2nd pass:
      ;;  Process the files and define the actual Vars
      (into (array-map)
            (walk-dir root-ns dir
                      (fn [sym ^File file] (defspin* sym file fnc)))))))

(defn eval-spin [spinstr]
  (-> spinstr compile-str apply-str))
