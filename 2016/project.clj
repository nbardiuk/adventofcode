(defproject aoc2016 "0.1.0-SNAPSHOT"
  :description "Solutions for https://adventofcode.com/2016"
  :url "https://github.com/nbardiuk/adventofcode"
  :profiles {:dev {:dependencies [[criterium "0.4.4"]
                                  [net.totakke/libra "0.1.1"]]
                   :plugins [[net.totakke/lein-libra "0.1.2"]]
                   :libra {:bench-paths ["bench"]}
                   }}
  :dependencies [[org.clojure/clojure "1.10.0"]])
