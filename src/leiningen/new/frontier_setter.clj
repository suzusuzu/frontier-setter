(ns leiningen.new.frontier-setter
  (:require [leiningen.new.templates :refer [renderer
                                             name-to-path
                                             ->files
                                             multi-segment
                                             sanitize-ns
                                             project-name
                                             year
                                             date]]
            [leiningen.core.main :as main]))

(def render (renderer "frontier-setter"))

(defn frontier-setter
  "FIXME: write documentation"
  [name]
  (let [main-ns (multi-segment (sanitize-ns name))
        data {:raw-name name
              :name (project-name name)
              :namespace main-ns
              :nested-dirs (name-to-path main-ns)
              :year (year)
              :date (date)}]
    (main/info "Generating fresh 'lein new' frontier-setter project.")
    (->files data
             ["project.clj" (render "project.clj" data)]
             ["conf.clj" (render "conf.clj" data)]
             [".gitignore" (render "gitignore" data)]
             [".hgignore" (render "hgignore" data)]
             ["README.md" (render "README.md" data)]
             ["src/{{nested-dirs}}.clj" (render "core.clj" data)]
             )))
