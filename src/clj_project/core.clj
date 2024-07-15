(ns clj-project.core
  (:gen-class))

(require '[clojure.core.match :refer [match]]
         '[clojure.java.io :as io])

(defrecord Env [parent defs])

(defn predef-env []
  (->Env nil {'inc inc
              'dec dec
              '+ +
              '- -
              '* *
              '/ /}))

(defn env-get
  [env name]
  (cond (contains? (:defs env) name) (get-in env [:defs name])
        (not (nil? (:parent env))) (env-get (:parent env) name)
        :else (throw
               (ex-info
                (format "Undefined symbol: %s." name)
                {:symbol name}))))

(defn env-set
  [env name value]
  (assoc-in env [:defs name] value))

(defn env-update
  [env name value]
  (if (contains? (:defs env) name)
    (assoc-in env [:defs name] value)
    (if (not (nil? (:parent env)))
      (assoc env :parent (env-update (:parent env) name value))
      (throw (ex-info (format "Undefined symbol: %s." name) {:symbol name})))))

(def global-env (atom (predef-env)))

(defn avalia
  ([x] (avalia x @global-env))
  ([x env]
   (match [x]
     [(n :guard number?)] n
     [(t :guard string?)] t
     [(b :guard boolean?)] b
     [(v :guard symbol?)] (env-get env v)
     [(['quote q] :seq)] q
     [(['if test then else] :seq)] (if (avalia test env)
                                     (avalia then env)
                                     (avalia else env))
     ;; Abstração
     [(['fun [formal-arg] body] :seq)]
     (fn [actual-arg]
       (avalia body
               (->Env env {formal-arg actual-arg})))
     ;; Abstração com múltiplos argumentos
     [(['fun [& fargs] body] :seq)]
     (fn [& aargs]
       (if (= (count fargs) (count aargs))
         (avalia body (->Env env
                             (into {} (map vector fargs aargs))))
         (throw
          (ex-info (format
                    "Arity error. Expected: %d. Found: %d."
                    (count fargs)
                    (count aargs))
                   {}))))
     ;; Definição
     [(['def name value] :seq)]
     (let [evaluated-value (avalia value env)
           updated-env (env-set env name evaluated-value)]
       (reset! global-env updated-env)
       [updated-env nil])
     ;; Programação imperativa (set!)
     [(['set! name value] :seq)]
     (let [evaluated-value (avalia value env)
           updated-env (env-update env name evaluated-value)]
       (reset! global-env updated-env)
       evaluated-value)
     ;; Aplicação
     [([rator rand] :seq)]
     (let [rator-val (avalia rator env)
           rand-val (avalia rand env)]
       (rator-val rand-val))
     ;; Aplicação com múltiplos argumentos
     [([rator & rands] :seq)]
     (let [rator-val (avalia rator env)
           rand-vals (for [rand rands] (avalia rand env))]
       (apply rator-val rand-vals)))))

(defn -main
  [& args]
  (let [file (first args)]
    (when (nil? file)
      (println "Usage: java -jar target\\uberjar\\clj-project-0.1.0-SNAPSHOT-standalone.jar arquivo-de-teste.clj-project")
      (System/exit 1))
    (let [content (slurp (io/file file))]
      (doseq [expr (read-string (str "[" content "]"))]
        (println (avalia expr))))))