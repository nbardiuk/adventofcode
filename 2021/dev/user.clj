(ns user
  (:require [clojure.test :as test]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clj-http.lite.client :as http]))

(defn run-tests []
  (refresh)
  (test/run-all-tests #"day.*"))

(def session "get-from-browser")

(defn fetch-input [day]
  (let [input (-> (str "https://adventofcode.com/2021/day/" day "/input")
                  (http/get {:headers {"Cookie" (str "session=" session)}})
                  :body)]
    (spit (str "resources/input" day ".txt") input)
    input))

(comment
  (fetch-input 1)
  #__)
