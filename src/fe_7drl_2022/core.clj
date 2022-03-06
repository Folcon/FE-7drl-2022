(ns fe-7drl-2022.core
  (:require
   [clojure.string :as str]
   [environ.core :refer [env]]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll EventKey Window]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint]))


(set! *warn-on-reflection* true)

(defn debug? [] (= (env :debug?) "true"))

(defonce *window (atom nil))

(def ^Typeface face-default
  (.matchFamiliesStyle (FontMgr/getDefault) (into-array String [".SF NS", "Helvetica Neue", "Arial"]) FontStyle/NORMAL))

(def typeface face-default)

(defonce *floating (atom false))

(add-watch *floating ::window
  (fn [_ _ _ floating]
    (when-some [window @*window]
      (if floating
        (window/set-z-order window :floating)
        (window/set-z-order window :normal)))))

(def padding 4)

(def dictionary (set ["apple"]))

(def solutions ["APPLE"])

(defn empty-state []
  {:word    (rand-nth solutions)
   :guesses []
   :typing  ""})

(def *state (atom (empty-state)))

(defn won? [{:keys [word guesses]}]
  (= (last guesses) word))

(defn type [code]
  (let [{:keys [typing] :as state} @*state
        typed (count typing)]
    (when-not (won? @*state)
      (cond
        (and (< typed 5) (contains? (into #{} (map str) "ABCDEFGHIJKLMNOPQRSTUVWXYZ") code))
        (swap! *state update :typing str code)

        (and (> typed 0) (= "Backspace" code))
        (swap! *state update :typing subs 0 (dec typed))

        (and (= 5 typed) (= "Enter" code))
        (if (contains? dictionary typing)
          (swap! *state #(-> % (assoc :typing "") (update :guesses conj typing))))))))

(defn color [word letter idx]
  (cond
    (= (str (nth word idx)) (str letter)) :green
    (str/includes? word (str letter)) :yellow
    :else :gray))

(defn merge-colors [a b]
  (cond
    (= :green b)  :green
    (= :green a)  :green
    (= :yellow b) :yellow
    (= :yellow a) :yellow
    :else         :gray))

(defn colors [word guesses]
  (apply merge-with merge-colors {}
    (for [guess guesses
          [letter idx] (map vector guess (range))]
      {(str letter) (color word letter idx)})))

(def field
  (ui/dynamic ctx [{:keys [font-large stroke-light-gray stroke-dark-gray fill-green fill-yellow fill-dark-gray fill-white fill-black]} ctx
                   {:keys [word guesses typing] :as state} @*state]
    (let [fill (fn [letter idx]
                 (case (color word letter idx)
                   :green  fill-green
                   :yellow fill-yellow
                   :gray   fill-dark-gray))]
      (ui/column
        (interpose (ui/gap 0 padding)
          (for [guess guesses]
            (ui/row
              (interpose (ui/gap padding 0)
                (for [[letter idx] (map vector guess (range))]
                  (ui/fill (fill letter idx)
                    (ui/width 50
                      (ui/halign 0.5
                        (ui/height 50
                          (ui/valign 0.5
                            (ui/label (str letter) font-large fill-white)))))))))))
        (when-not (won? state)
          (let [colors (colors word guesses)]
            (list
              (ui/gap 0 padding)
              (ui/row
                (interpose (ui/gap padding 0)
                  (for [idx (range 0 5)]
                    (if-some [letter (when (< idx (count typing))
                                       (str (nth typing idx)))]
                      (ui/fill stroke-dark-gray
                        (ui/width 50
                          (ui/halign 0.5
                            (ui/height 50
                              (ui/valign 0.5
                                (let [color (cond
                                              (= :gray (colors letter))
                                              fill-dark-gray

                                              (some #(= (str (nth % idx)) letter) guesses)
                                              (fill letter idx)

                                              (some? (colors letter))
                                              fill-yellow

                                              :else
                                              fill-black)]
                                  (ui/label letter font-large color)))))))
                      (ui/fill stroke-light-gray
                        (ui/gap 50 50)))))))))))))

(defn key
  ([char] (key char {:width 25 :code char}))
  ([char {:keys [width code]}]
   (ui/clickable
     #(type code)
     (ui/dynamic ctx [{:keys [font-small fill-green fill-yellow fill-dark-gray fill-light-gray fill-black fill-white]} ctx
                      color (get (:colors ctx) char)]
       (ui/fill
         (case color
           :green  fill-green
           :yellow fill-yellow
           :gray   fill-dark-gray
           nil     fill-light-gray)
         (ui/width width
           (ui/halign 0.5
             (ui/height 35
               (ui/valign 0.5
                 (ui/label char font-small (if (some? color) fill-white fill-black)))))))))))

(def keyboard
  (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black]} ctx
                   {:keys [word guesses]} @*state]
    (ui/with-context {:colors (colors word guesses)}
      (ui/column
        (ui/gap 0 padding)
        (ui/halign 0.5
          (ui/row
            (interpose (ui/gap padding 0)
              (map #(key (str %)) "ABCDEFGHIJ"))))
        (ui/gap 0 padding)
        (ui/halign 0.5
          (ui/row
            (interpose (ui/gap padding 0)
              (map #(key (str %)) "KLMNOPQRST"))))
        (ui/gap 0 padding)
        (ui/halign 0.5
          (ui/row
            (key "⏎" {:width (+ (* 2 25) padding), :code "Enter"})
            (ui/gap padding 0)
            (interpose (ui/gap padding 0)
              (map #(key (str %)) "UVWXYZ"))
            (ui/gap padding 0)
            (key "⌫" {:width (+ (* 2 25) padding), :code "Backspace"})))))))

(def ui
  (ui/on-key-down #(type (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx]
        (let [font-small (Font. ^Typeface typeface (float (* scale 13)))
              fill-black (paint/fill 0xFF000000)
              fill-light-gray (paint/fill 0xFFD4D6DA)]
          (ui/with-context
            {:font-large      (Font. ^Typeface typeface (float (* scale 26)))
             :font-small      font-small
             :fill-white      (paint/fill 0xFFFFFFFF)
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  (paint/fill 0xFF777C7E)
             :fill-green      (paint/fill 0xFF6AAA64)
             :fill-yellow     (paint/fill 0xFFC9B457)
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              (ui/halign 0.5
                (ui/clickable
                  #(reset! *state (empty-state))
                  (ui/padding 10 10
                    (ui/label "↻ Reset" font-small fill-black))))
              (ui/gap 0 padding)
              [:stretch 1 nil]
              (ui/halign 0.5 field)
              [:stretch 1 nil]
              (ui/gap 0 padding)
              (ui/halign 0.5 keyboard))))))))

(defn checkbox [*checked text]
  (ui/clickable
    #(swap! *checked not)
    (ui/dynamic ctx [checked @*checked
                     {:keys [font-ui fill-text leading scale]} ctx]
      (let [border (doto (Paint.)
                     (.setColor (unchecked-int 0xFF000000))
                     (.setMode PaintMode/STROKE)
                     (.setStrokeWidth (* 1 scale)))]
        (ui/row
          (ui/fill border
            (if checked
              (ui/padding 1 1
                (ui/fill fill-text
                  (ui/gap (- leading 2) (- leading 2))))
              (ui/gap leading leading)))
          (ui/gap 5 0)
          (ui/label text font-ui fill-text))))))

(def *selected (atom nil))

(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-ui   (Font. face-default (float (* 13 scale)))
          leading   (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/with-context {:face-ui   face-default
                        :font-ui   font-ui
                        :leading   leading
                        :fill-text fill-text}
        (ui/row
          (ui/column
            [:stretch 1
             (ui/vscrollbar
               (ui/vscroll
                 (ui/column
                   (for [[name ui] (sort-by first {"Apple" 1 "Pear" 2})]
                     (ui/clickable
                       #(reset! *selected name)
                       (ui/dynamic ctx [selected? (= name @*selected)
                                        hovered?  (:hui/hovered? ctx)]
                         (let [label (ui/padding 20 leading
                                       (ui/label name font-ui fill-text))]
                           (cond
                             selected? (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFB2D7FE))) label)
                             hovered?  (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA))) label)
                             :else     label))))))))]
            (ui/padding 10 10
              (checkbox *floating "On top")))
          [:stretch 1
           ui])))))

(defn on-paint [window ^Canvas canvas]
  (.clear canvas (unchecked-int 0xFFF6F6F6))
  (let [bounds (window/content-rect window)
        ctx    {:scale (window/scale window)}]
    (profile/reset)
    ; (profile/measure "frame"
    (ui/draw app ctx bounds canvas)
    (profile/log)
    #_(window/request-frame window)))

(some-> @*window window/request-frame)

(defn on-event [window event]
  (let [changed? (condp instance? event
                   EventMouseMove
                   (let [pos   (IPoint. (.getX ^EventMouseMove event) (.getY ^EventMouseMove event))
                         event {:hui/event :hui/mouse-move
                                :hui.event/pos pos}]
                     (ui/event app event))

                   EventMouseButton
                   (let [event {:hui/event :hui/mouse-button
                                :hui.event.mouse-button/is-pressed (.isPressed ^EventMouseButton event)}]
                     (ui/event app event))

                   EventMouseScroll
                   (ui/event app
                     {:hui/event :hui/mouse-scroll
                      :hui.event.mouse-scroll/dx (.getDeltaX ^EventMouseScroll event)
                      :hui.event.mouse-scroll/dy (.getDeltaY ^EventMouseScroll event)})

                   EventKey
                   (ui/event app
                     {:hui/event (if (.isPressed ^EventKey event) :hui/key-down :hui/key-up)
                      :hui.event.key/key (.getName (.getKey ^EventKey event))})

                   nil)]
    (when changed?
      (window/request-frame window))))

(defn make-window []
  (let [screen (last (hui/screens))
        scale  (:scale screen)
        width  (* 600 scale)
        height (* 400 scale)
        area   (:work-area screen)
        x      (:x area)
        y      (-> (:height area) (- height) (/ 2) (+ (:y area)))]
    (doto
      (window/make
        {:on-close #(reset! *window nil)
         :on-paint #'on-paint
         :on-event #'on-event})
      (window/set-title "Fruit Economy 👋 - 7DRL 2022")
      (window/set-window-size width height)
      (window/set-window-position x y)
      (window/set-visible true))))

(defn -main [& args]
  ;; TODO: Display somewhere in the UI
  (println (str "VERSION: " (env :game-version) (when (debug?) "\nDEBUG BUILD")))
  (when (debug?)
    ;; Swap to require and resolve in one step!
    (future (apply (requiring-resolve 'nrepl.cmdline/-main) args)))
  (hui/start #(reset! *window (make-window))))


(comment
  (do
    (hui/doui (some-> @*window window/close))
    (reset! *floating false)
    (reset! *window (hui/doui (make-window))))

  (hui/doui (window/set-z-order @*window :normal))
  (hui/doui (window/set-z-order @*window :floating)))
