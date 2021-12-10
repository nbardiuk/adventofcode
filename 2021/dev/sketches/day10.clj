(ns sketches.day10
  (:require [clojure2d.core :as c2d]
            [clojure.string :as string]
            [day10-test :refer [example]]
            [day10 :refer [config]]))

(def lines (string/split-lines example))

(defn analyze [brackets]
  (loop [[bracket & brackets :as queue] brackets
         valid []
         open '()]
    (if bracket
      (if-let [open-bracket ((comp :open config) bracket)]
        (if (= open-bracket (peek open))
          (recur brackets (conj valid bracket) (pop open))
          {:line valid
           :unexpected queue
           :message (str " - Expected " ((comp :closing config) (peek open)) ", but got " bracket " instead")})
        (recur brackets (conj valid bracket) (conj open bracket)))
      {:line valid
       :completion (map (comp :closing config) open)})))

(def steps (vec (for [y (range (count lines))
                      :let [line (nth lines y)]
                      x (range (count line))
                      :let [lines (concat (take y lines)
                                          [(subs line 0 x)])]]

                  (for [line lines]
                    (analyze line)))))

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (c2d/set-font canvas "Iosevka")
  (c2d/set-font-attributes canvas 30.)
  (let [index (mod frameno (count steps))
        lines (nth steps index)]
    (doseq [j (range (count lines))
            :let [{:keys [line completion unexpected message]} (nth lines j)
                  y (* (inc j) 60)]]

      (c2d/set-color canvas "#ffff66")
      (doseq [i (range (count line))
              :let [bracket (nth line i)
                    x (* (inc i) 20)]]
        (c2d/text canvas bracket x y))

      (c2d/set-color canvas "#ff0000")
      (doseq [i (range (count unexpected))
              :let [bracket (nth unexpected i)
                    x (* (+ (inc i) (count line)) 20)]]
        (c2d/text canvas bracket x y))
      (c2d/set-color canvas "#6d523d")
      (c2d/text canvas message (* (+ 1 (count line) (count unexpected)) 20) y)

      (c2d/set-color canvas "#9999cc")
      (doseq [i (range (count completion))
              :let [bracket (nth completion i)
                    x (* (+ (inc i) (count line)) 20)]]
        (c2d/text canvas bracket x y)))

    (c2d/rect canvas
              (* (+ (count (:line (last lines)))
                    (count (:unexpected (last lines)))) 20)
              (* (count lines) 60)
              15 4)
    #_(c2d/save-image (c2d/get-image canvas) (format "images/day10-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1000 650 :highest)
    :window-name "day10"
    :hint :highest
    :draw-fn draw}))
