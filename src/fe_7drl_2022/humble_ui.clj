(ns fe-7drl-2022.humble-ui
  (:require [io.github.humbleui.core :refer [deftype+]]
            [io.github.humbleui.protocols :refer [IComponent -measure -draw -event]])
  (:import [io.github.humbleui.skija Canvas]
           [io.github.humbleui.types IPoint IRect Rect]))


;; Should be in io.github.humbleui.ui
(defn fragment [& children]
  (list children))

(def <> #'fragment)


(deftype+ CustomUI [width height on-paint on-event ^:mut child-rect]
  IComponent
  (-measure [_ ctx cs]
    (IPoint. width height))
  (-draw [_ ctx cs ^Canvas canvas]
    (set! child-rect (IRect/makeXYWH 0 0 (:width cs) (:height cs)))
    (when on-paint
      (let [canvas ^Canvas canvas
            {:keys [width height]} cs
            layer  (.save canvas)
            rect  (Rect/makeXYWH 0 0 width height)]
        (try
          (on-paint canvas width height)
          (finally
            (.restoreToCount canvas layer))))))
  (-event [_ event]
    (when on-event
      (on-event event))))

(defn custom-ui
  "(custom-ui 400 300 {:on-paint #'on-paint-impl
                       :on-event #'on-event-impl})"
  [width height {:keys [on-paint on-event]}]
  (->CustomUI width height on-paint on-event nil))
;; END Should be in io.github.humbleui.ui
