(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [eftest.runner :as eftest]))

(defn- only? [x]
  (or (-> x meta :only)
      (-> x meta :ns meta :only)))

(defn- focus [xs]
  (let [focused (filter only? xs)]
    (if (seq focused)
      focused
      xs)))

(defn run-tests
  "Run all tests"
  []
  (refresh)
  (eftest/run-tests (focus (eftest/find-tests "test"))
                    {:capture-output? false}))
