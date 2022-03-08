(ns fe-7drl-2022.components
  (:require [io.github.humbleui.ui :as ui]
            [fe-7drl-2022.humble-ui :as c-hui])
  (:import [io.github.humbleui.skija Canvas Paint PaintMode]))


(defn draw-radio-toggle-impl [fill scale]
  (fn [^Canvas canvas _window-width _window-height]
    (let [border (doto (Paint.)
                   (.setColor (unchecked-int 0xFF000000))
                   (.setMode PaintMode/STROKE)
                   (.setStrokeWidth (* 1 scale)))]
      (.drawCircle canvas 5 5 10 border)
      (.drawCircle canvas 5 5 6 fill))))

(defn radio-button [state value selected-path child]
  (ui/clickable
    #(swap! state assoc-in selected-path value)
    (ui/dynamic ctx [{:keys [fill-green fill-black scale]} ctx
                     selected (get-in @state selected-path)]
      (ui/column
        (ui/row
          (c-hui/custom-ui 20 20
            {:on-paint (#'draw-radio-toggle-impl (if (= selected value) fill-black fill-green) scale)}))
        (ui/gap 5 0)
        child))))
