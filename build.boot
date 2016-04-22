(set-env!
  :source-paths #{"src/cljs"}
  :resource-paths #{"html"}

  :dependencies '[[org.clojure/clojure "1.7.0"]
                  [org.clojure/clojurescript "1.7.170"]
                  [adzerk/boot-cljs "1.7.170-3"]
                  [pandeiro/boot-http "0.7.0"]
                  [adzerk/boot-reload "0.4.2"]
                  [rm-hull/monet "0.2.2"]
                  [jayq "2.5.4"]
                  ; [adzerk/boot-cljs-repl "0.3.0"]
                  ; [com.cemerick/piggieback "0.2.1"]     ;; needed by bREPL
                  ; [weasel "0.7.0"]                      ;; needed by bREPL
                  ; [org.clojure/tools.nrepl "0.2.12"]    ;; needed by bREPL
                  ])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[pandeiro.boot-http :refer [serve]]
         ; '[adzerk.boot-reload :refer [reload]]
         ; '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         )

(deftask dev
  "Launch development environment"
  []
  (comp
    (serve :dir "target" :port 8080)
    (watch)
    (cljs)
    (target :dir #{"target"})))
