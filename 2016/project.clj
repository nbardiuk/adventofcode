(defproject aoc2016 "0.1.0-SNAPSHOT"
  :description "Solutions for https://adventofcode.com/2016"
  :url "https://github.com/nbardiuk/adventofcode"
  :profiles {:dev {:dependencies [[criterium "0.4.4"]
                                  [net.totakke/libra "0.1.1"]
                                  [reloaded.repl "0.2.4"]
                                  [eftest "0.5.9"]]
                   :plugins [[net.totakke/lein-libra "0.1.2"]]
                   :source-paths ["dev"]
                   :libra {:bench-paths ["bench"]}
                   :repl-options {:init-ns user}}}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :plugins [[lein-eftest "0.5.9"]]
  :aliases {"test" ["eftest"]})
