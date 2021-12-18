(ns sketches.day18
  (:require [clojure2d.core :as c2d]
            [clojure.string :as string]
            [day00 :refer [fix-point take-until-repeat]]
            [day18 :refer [flat-index explode split]]))

(def steps
  (->>
   [[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
    [[2,[2,2]],[8,[8,1]]]]
   flat-index
   vector
   (fix-point
    (fn [steps]
      (into (pop steps)
            (->> (iterate explode (peek steps))
                 (take-until-repeat)
                 vec
                 ((fn [steps] (let [lst (peek steps)
                                    sp (split lst)]
                                (if (= lst sp)
                                  steps
                                  (conj steps sp)))))))))))

(count steps)

(defn draw [canvas _ ^long frameno _]
  (c2d/set-background canvas "#0f0f23")
  (c2d/set-font canvas "Iosevka")
  (let [index (mod frameno (count steps))
        scene (nth steps index)
        scene (->> scene
                   (map (fn [[v path]] [v path (- 6 (count path))]))
                   (reduce (fn [result [v path w]]
                             (conj result [v path w (+ w (or (last (last result)) 0))]))
                           []))
        cw (/ (c2d/width canvas) (last (last scene)))
        by (/ (c2d/height canvas) 2.)
        ch 20]
    (c2d/set-font-attributes canvas 15.)

    (doseq [[v path w sw] scene]
      (c2d/set-color canvas "#9999cc")
      (c2d/rect canvas
                (* (- sw w) cw) (- by (* ch (count path)))
                (* w cw) (* ch (count path))
                true))

    (doseq [[v path w sw] scene]
      (c2d/set-color canvas (cond
                              (= 5 (count path)) "#33ff33"
                              (<= 10 v) "#ff0000"
                              :else "#ffff66"))

      (let [d (* cw (/ v 20.))]
        (c2d/ellipse
         canvas
         (* (+ (- sw w) (/ w 2.)) cw)
         (- by (* ch (count path)) (/ d 2.))
         d d))

      (c2d/set-color canvas "#00c8ff")
      (let [text (if (= :left (last path))
                   (str (string/join (repeat (->> path reverse (take-while #{:left}) count) "["))
                        v)
                   (str v
                        (string/join (repeat (->> path reverse (take-while #{:right}) count) "]"))))]
        (c2d/text canvas text
                  (+ (* (- sw w) cw)
                     (if (= :right (last path))
                       (- (* w cw) (c2d/text-width canvas text))
                       0))
                  (+ by ch))))

    #_(c2d/save-image (c2d/get-image canvas) (format "images/day18-%03d.jpg" index))))

(comment
  (c2d/show-window
   {:canvas (c2d/canvas 1024 768 :highest)
    :window-name "day18"
    :hint :highest
    :draw-fn #'draw}))
