(ns user
  (:require
    [clj-http.lite.client :as http]))


(def session "get-from-browse")


(defn fetch-input [day]
  (try
    (let [input (-> (http/request
                      {:method :get
                       :url (str "https://adventofcode.com/2024/day/" day "/input")
                       :headers {"cookie" (str "session=" session)}})
                  :body)]
         (spit (format "resources/input%02d.txt" day) input)
         (println input))
    (catch Exception e e)))


(comment
  (fetch-input 1)
  #__)
