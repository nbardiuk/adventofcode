(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [eftest.runner :as eftest]))

(defn run-tests
  "Run all tests"
  []
  (refresh)
  (eftest/run-tests (eftest/find-tests "test")
                    {:capture-output? false}))
