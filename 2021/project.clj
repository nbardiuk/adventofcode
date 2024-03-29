(defproject aoc-2021 "0.1.0-SNAPSHOT"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0-alpha3"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :profiles {:dev {:dependencies [[org.clj-commons/clj-http-lite "0.4.392"]
                                  [org.clojure/tools.namespace "0.2.11"]
                                  [io.github.nextjournal/clerk "0.2.214"]
                                  [quil "3.1.0"]
                                  [clojure2d "1.4.3"]]
                   :source-paths ["dev"]
                   :repl-options {:init-ns user}}})
