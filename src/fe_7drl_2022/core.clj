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

(def tiles [:beach :desert :forest :grassland :jungle :mountain :snow-mountain :ocean :snow :tundra])

(def units
  {:growing-plant "🌱" :dying-plant "🥀"
   :bush "🌳" :tree-1 "🌴" :tree-2 "🌲" :tree-3 "🌵" :tree-4 "🌾" :tree-5 "🎋" :tree-6 "🎍" :magic-tree "🎄"
   :flower-1 "🌸" :flower-2 "💮" :flower-3 "🏵️" :flower-4 "🌺" :flower-5 "🌻" :flower-6 "🌼" :flower-7 "🌷"
   :herb-1 "🌿" :herb-2 "☘️" :herb-3 "🍀" :herb-4 "🍁" :shroom "🍄" :nut-1 "🌰" :nut-2 "🥥"
   :fruit-1 "🍇" :fruit-2 "🍈" :fruit-3 "🍉" :fruit-4 "🍊" :fruit-5 "🍋" :fruit-6 "🍌" :fruit-7 "🍍" :fruit-8 "🥭"
   :fruit-9 "🍎" :fruit-10 "🍏" :fruit-11 "🍐" :fruit-12 "🍑" :fruit-13 "🍒" :fruit-14 "🍓" :fruit-15 "🥝" :fruit-16 "🍅"
   :rabbit "🐇" :deer "🦌" :dragon "🐉" :spider "🕷️"})

(defn rand-tiles [] (vec (take 15 (shuffle tiles))))

(defn rand-tile-seqs [n] (vec (repeatedly n rand-tiles)))

(defn selected-peeps [peeps] (into [] (comp (map second) (filter :peep/selected)) peeps))

(defn combat [{:keys [quests selected-quest peeps] :as state}]
  (let [quest (get quests selected-quest)
        peeps (selected-peeps peeps)
        _ (println :quest (pr-str quest))
        _ (println :peeps (pr-str peeps))]
    state))

(defn empty-state []
  {:word    (rand-nth solutions)
   :guesses (rand-tile-seqs 10)
   :typing  ""
   :units (into {} (map (juxt (juxt :x :y) identity)) (repeatedly 10 #(hash-map :x (inc (rand-int 15)) :y (inc (rand-int 10)) :glyph (rand-nth (vals units)))))
   :quests (into (sorted-map) {"Goblins Attack Farm!" {:quest/name "Goblins Attack Farm!" :quest/mobs [{:mob/name "Goblin" :combat/dice "2d4"} {:mob/name "Goblin" :combat/dice "2d4"} {:mob/name "Goblin" :combat/dice "2d4"}]}
                               "Cull Local Rats!" {:quest/name "Cull Local Rats!" :quest/mobs [{:mob/name "Rat" :combat/dice "1d4"} {:mob/name "Rat" :combat/dice "1d4"} {:mob/name "Rat" :combat/dice "1d4"}]}})
   :selected-quest "Cull Local Rats!"
   :peeps (into (sorted-map) {"peep 1" {:peep/name "peep 1" :peep/class :mage :peep/selected false :combat/dice "2d4"}
                              "peep 2" {:peep/name "peep 2" :peep/class :fighter :peep/selected false :combat/dice "2d4"}})
   :message-log ["Welcome to Fruit Economy!" "Have fun!"]})

(def *state (atom (empty-state)))

(comment
  (reset! *state (empty-state)))

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

(defn colour [word letter idx]
  (cond
    (and (< idx 5) (= (str (nth word idx)) (str letter))) :green
    (str/includes? word (str letter)) :yellow
    :else :gray))

(defn fill-colour
  "an unchecked-int or r g b should be between 0 and 255"
  ([uint] (unchecked-int uint))
  ([r g b]
   (unchecked-int
     (bit-or
       (unchecked-int 0xFF000000)
       (bit-shift-left r 16)
       (bit-shift-left g 8)
       (bit-shift-left b 0)))))

(defn render-tile-colour [terrain]
  (condp = terrain
    :beach (fill-colour 230 236 172) #_(rand-nth [(fill-colour 117 139 171) (fill-colour 230 236 172)])
    :desert (fill-colour 246 244 118)
    :forest (fill-colour 64 88 37)
    :grassland (fill-colour 78 138 53)
    :jungle (fill-colour 66 108 40)
    :mountain (fill-colour 73 66 52)
    :snow-mountain (fill-colour 86 82 73)
    :ocean (rand-nth [(fill-colour 87 119 197) (fill-colour 87 102 153) (fill-colour 87 102 153)])
    :snow (fill-colour 247 246 247)
    :tundra (fill-colour 153 153 155)
    (fill-colour 0 0 0)))

(defn merge-colors [a b]
  (cond
    (= :green b)  :green
    (= :green a)  :green
    (= :yellow b) :yellow
    (= :yellow a) :yellow
    :else         :gray))

(defn colours [word guesses]
  (apply merge-with merge-colors {}
    (for [guess guesses
          [letter idx] (map vector guess (range))]
      {(str letter) (colour word letter idx)})))

(def field
  (ui/dynamic ctx [{:keys [font-large stroke-light-gray stroke-dark-gray fill-green fill-yellow fill-dark-gray fill-white fill-black]} ctx
                   {:keys [word guesses units typing] :as state} @*state]
    (let [fill (fn [tile]
                 (paint/fill (render-tile-colour tile)))
          unit-glyph (fn [_tile x y] (get-in units [[x y] :glyph] " "))
          terrain guesses]
      (ui/column
        (interpose (ui/gap 0 padding)
          (for [[tile-row y-idx] (map vector terrain (range))]
            (ui/row
              (interpose (ui/gap padding 0)
                (for [[tile x-idx] (map vector tile-row (range))]
                  (ui/fill (fill tile)
                    (ui/width 50
                      (ui/halign 0.5
                        (ui/height 50
                          (ui/valign 0.5
                            (ui/label (unit-glyph tile x-idx y-idx) font-large fill-white)))))))))))
        #_
        (when-not (won? state)
          (let [colors (colours word guesses)]
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

(declare *selected-ui-view)

(defn quest-ui-btn
  ([quest-name] (quest-ui-btn quest-name {:width 100 :code quest-name}))
  ([quest-name {:keys [width code selected-quest]}]
   (ui/clickable
     #(do
        (reset! *selected-ui-view "Quest")
        (swap! *state assoc :selected-quest quest-name))
     (ui/dynamic ctx [{:keys [font-small fill-green fill-yellow fill-dark-gray fill-light-gray fill-black fill-white]} ctx
                      color (get (:colors ctx) quest-name)
                      color (if (= selected-quest quest-name) :green color)]
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
                 (ui/label quest-name font-small (if (some? color) fill-white fill-black)))))))))))

(defn checkbox [checked-path child]
  (ui/clickable
    #(swap! *state update-in checked-path not)
    (ui/dynamic ctx [checked (get-in @*state checked-path)
                     {:keys [font-ui fill-text leading scale]} ctx]
      (let [border (doto (Paint.)
                     (.setColor (unchecked-int 0xFF000000))
                     (.setMode PaintMode/STROKE)
                     (.setStrokeWidth (* 1 scale)))]
        (ui/column
          (ui/row
            (ui/fill border
              (if checked
                (ui/padding 1 1
                  (ui/fill fill-text
                    (ui/gap (- leading 2) (- leading 2))))
                (ui/gap leading leading))))
          (ui/gap 5 0)
          child)))))

(defn show-val-ui [v font fill]
  (cond
    (instance? clojure.lang.Atom v) (show-val-ui @v font fill)
    (vector? v) (ui/column (interpose (ui/gap 2 padding) (mapv #(show-val-ui % font fill) v)))
    (map? v) (ui/label (pr-str v) font fill)
    :else (ui/label (pr-str v) font fill)))

(defn show-map-ui [m font fill]
  (ui/column
    (for [[k v] m]
      (ui/padding 5
        (ui/row
          (ui/label (str k) font fill)
          (ui/gap 10 0)
          (show-val-ui v font fill))))))

(def quest-detail-ui
  (ui/dynamic ctx [{:keys [font-large font-small stroke-light-gray stroke-dark-gray fill-green fill-yellow fill-dark-gray fill-white fill-black]} ctx
                   {:keys [quests selected-quest peeps] :as state} @*state]
    (ui/column
      (show-map-ui (get quests selected-quest) font-large fill-black)
      (ui/gap 0 padding)
      (ui/halign 0.5
        (ui/row
          (interpose (ui/gap padding 2)
            (for [[_name peep] peeps]
              (ui/fill fill-yellow
                (ui/padding 10
                  (let [{:peep/keys [name]} peep]
                    (ui/column
                      (checkbox [:peeps name :peep/selected]
                        (show-map-ui peep font-small fill-black))))))))))
      (ui/gap 0 padding)
      (let [selected-peeps? (seq (selected-peeps peeps))]
        (ui/halign 0.5
          (ui/fill (if selected-peeps? fill-green fill-dark-gray)
            (ui/clickable
              (if selected-peeps? #(swap! *state combat) identity)
              (ui/padding 10 10
                (ui/label "⇫ Begin" font-small fill-white)))))))))


(def keyboard
  (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black]} ctx
                   {:keys [word guesses]} @*state]
    (ui/with-context {:colors (colours word guesses)}
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

(def message-log
  (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black]} ctx
                   {:keys [message-log]} @*state]
    (ui/column
      (ui/gap 0 padding)
      (ui/halign 0.5
        (ui/label "[ Message Log ]" font-small fill-black))
      (ui/gap 0 padding)
      (ui/halign 0.5
        (ui/halign 0.5
          (ui/column
            [:stretch 1
             (ui/column
               (interpose (ui/gap 2 padding)
                 (map #(ui/halign 0.5 (ui/label (str %) font-small fill-black)) message-log)))]))))))

(def map-ui-view
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
              (ui/halign 0.5
                (ui/dynamic ctx [_ (:selected-quest @*state)]
                  (ui/row
                    (interpose (ui/gap 2 padding)
                      (for [[name _quest] (:quests @*state)
                            :let [label-width (+ (* (count name) 8) padding)]]
                        (quest-ui-btn name {:width label-width :code name :selected-quest (:selected-quest @*state)}))))))
              (ui/gap 0 padding)
              [:stretch 1 nil]
              (ui/halign 0.5 field)
              [:stretch 1 nil]
              (ui/gap 0 padding)
              (ui/halign 0.5 message-log))))))))

(def quest-ui-view
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
              (ui/halign 0.5
                (ui/dynamic ctx [_ (:selected-quest @*state)]
                  (ui/row
                    (interpose (ui/gap 2 padding)
                      (for [[name _quest] (:quests @*state)
                            :let [label-width (+ (* (count name) 8) padding)]]
                        (quest-ui-btn name {:width label-width :code name :selected-quest (:selected-quest @*state)}))))))
              (ui/gap 0 padding)
              [:stretch 1 nil]
              (ui/halign 0.5 quest-detail-ui)
              [:stretch 1 nil]
              (ui/gap 0 padding)
              (ui/halign 0.5 message-log))))))))

(def peep-ui-view
  (ui/on-key-down #(type (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx
                       peeps (:peeps @*state)]
        (let [font-small (Font. ^Typeface typeface (float (* scale 13)))
              fill-black (paint/fill 0xFF000000)
              fill-yellow (paint/fill 0xFFC9B457)
              fill-light-gray (paint/fill 0xFFD4D6DA)]
          (ui/with-context
            {:font-large      (Font. ^Typeface typeface (float (* scale 26)))
             :font-small      font-small
             :fill-white      (paint/fill 0xFFFFFFFF)
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  (paint/fill 0xFF777C7E)
             :fill-green      (paint/fill 0xFF6AAA64)
             :fill-yellow     fill-yellow
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              (ui/halign 0.5
                (ui/row
                  (interpose (ui/gap padding 2)
                    (for [[_name peep] peeps]
                      (ui/fill fill-yellow
                        (ui/padding 10
                          (let [{:peep/keys [name]} peep]
                            (ui/column
                              (checkbox [:peeps name :peep/selected]
                                (show-map-ui peep font-small fill-black)))))))))))))))))


(def ui-views
  ;; exploiting the fact that as long as array-map doesn't grow, it keeps insertion order
  (array-map
    "Map" map-ui-view
    "Quest" quest-ui-view
    "Peep" peep-ui-view))

(def *selected-ui-view (atom (ffirst ui-views)))

(defn atom-checkbox [*checked text]
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
                   (for [[name _ui] ui-views]
                     (ui/clickable
                       #(reset! *selected-ui-view name)
                       (ui/dynamic ctx [selected? (= name @*selected-ui-view)
                                        hovered?  (:hui/hovered? ctx)]
                         (let [label (ui/padding 20 leading
                                       (ui/label name font-ui fill-text))]
                           (cond
                             selected? (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFB2D7FE))) label)
                             hovered?  (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA))) label)
                             :else     label))))))))]
            (ui/padding 10 10
              (atom-checkbox *floating "On top")))
          [:stretch 1
           (ui/dynamic _ [name @*selected-ui-view]
             (ui-views name))])))))

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
        width  (* 1200 scale)
        height (* 800 scale)
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
