(ns user
  (:require [clojure.test :as t]
            [clojure.tools.namespace.repl :refer [refresh]]))

(defn run-tests [& _]
  (refresh)
  (t/run-all-tests #"day.*"))
