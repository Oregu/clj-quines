(defproject quines "0.3.0"
  :description "Translating miniKanren quines relational interpreter to clojure core.logic."
  :url "http://github.com/Oregu/clj-quines"

  :source-paths ["src/clj/"]
  :resource-paths ["res"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.5"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [compojure "1.1.6"]]

  :plugins [[lein-cljsbuild "1.0.1"]
            [lein-ring "0.8.8"]]

  :cljsbuild {
    :builds [{
      :source-paths ["src/cljs/"]
      :compiler {
        :output-dir "target/cljs"
        :output-to "res/public/js/hello.js"
        :optimizations :whitespace ; :whitespace, :simple, :advanced
        :pretty-print true}}]}

  :ring {:handler quines.http.server/handler})
