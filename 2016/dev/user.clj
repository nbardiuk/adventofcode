(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [eftest.runner :as eftest]))

(defn- fast? [x]
  (-> x meta :slow not))

(defn run-tests
  "Run all tests"
  []
  (refresh)
  (eftest/run-tests (filter fast? (eftest/find-tests "test"))
                    {:capture-output? false}))
