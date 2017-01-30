(ns {{namespace}}
  (:use [{{sanitized}}.util.dsl])
  (:use [{{sanitized}}.util.gp])
  (:gen-class))

(declare conf)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (binding [*ns* (the-ns '{{namespace}})]
      (load-file "conf.clj")))