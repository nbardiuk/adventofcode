(ns user
  (:require
   [clj-http.lite.client :as http]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :refer [refresh]]
   [nextjournal.clerk :as clerk]))

(defn run-tests
  ([] (run-tests ".*"))
  ([day]
   (let [result (refresh)]
     (if (= :ok result)
       (test/run-all-tests (re-pattern (str "day" day "-test")))
       result))))

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

(comment
  (clerk/serve! {:watch-paths ["dev"]})
  #__)
