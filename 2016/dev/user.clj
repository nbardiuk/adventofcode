(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [eftest.runner :as eftest]))

(defn- only? [test]
  (or (-> test meta :only)
      (-> test meta :ns meta :only)))

(defn- focus [tests]
  (let [focused (filter only? tests)]
    (if (seq focused)
      focused
      tests)))

(defn run-tests
  "Run all tests"
  []
  (refresh)
  (eftest/run-tests (focus (eftest/find-tests "test"))
                    {:capture-output? false}))
