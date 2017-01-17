(ns {{sanitized}}.util.dsl
  (:gen-class))

(defmacro defconf
  [& args]
  `(let [m# (hash-map ~@args)]
     (println m#)))