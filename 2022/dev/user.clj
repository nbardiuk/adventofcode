(ns user
  (:require
   [clj-http.lite.client :as http]))

(def session "get-from-browse")

(defn fetch-input [day]
  (let [input (-> (http/request
                   {:method  :get
                    :url     (str "https://adventofcode.com/2022/day/" day "/input")
                    :headers {"cookie" (str "session=" session)}})
                  :body)]
    (spit (format "test-resources/input%02d.txt" day) input)
    input))

(comment
  (fetch-input 1)
  #__)
