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

(defn str->int [n]
  (Integer/parseInt n))

(defn basic-roll [n d]
  (repeatedly n #(inc (rand-int d))))

(defn roll
  "Takes dice of the form :2d6"
  [dice]
  (let [dice (if (keyword? dice) (name dice) dice)
        [_ n d] (re-matches #"(\d+)d(\d+)" dice)
        [n d] [(str->int n) (str->int d)]]
    (basic-roll n d)))

(defn selected-peeps [peeps] (into [] (comp (map second) (filter :peep/selected)) peeps))

(defn peep? [entity]
  (contains? entity :peep/name))

(defn mob? [entity]
  (contains? entity :mob/name))

(defn combat-round [state initiative]
  (reduce
    (fn [{:keys [combatants] :as state} entity]
      (let [p? (peep? entity)
            atker-name (get entity (if p? :peep/name :mob/name))

            atker (get combatants atker-name)

            [target-name target] (first (shuffle (filter (comp (if p? mob? peep?) second) combatants)))

            ;; combat round
            hit-roll (reduce + (roll (:combat/hit atker)))
            target-def (:combat/def target)
            hits? (> hit-roll target-def)
            dmg-roll (reduce + (roll (:combat/dmg atker)))
            target-hp (:combat/hp target)
            rem-hp (- target-hp dmg-roll)]
        (cond->
          (update state :messages conj (str atker-name " attacks " target-name " and " (if hits? "hits" "misses") " (" hit-roll " vs " target-def ")" (when hits? (str " dealing " dmg-roll " dmg and " (if (> rem-hp 0) (str "leaving them on " rem-hp " hp") "killing them")))))
          hits?
          (assoc-in [:combatants target-name :combat/hp] rem-hp))))
    state
    initiative))

(defn alive-on-each-side [combatants]
  (frequencies (into [] (comp (map second) (remove #(>= 0 (:combat/hp %))) (map #(cond (peep? %) :peep (mob? %) :mob :else :other))) combatants)))

(defn combat-encounter [peeps mobs]
  (let [init-initiative (shuffle (into peeps mobs))
        init-combat-state {:messages []
                           :combatants (into {} (map (juxt #(or (:peep/name %) (:mob/name %)) identity)) init-initiative)}]
    (println :init-initiative (pr-str init-initiative))
    (loop [round 0
           combat-state init-combat-state
           initiative init-initiative]
      (let [round-result (combat-round combat-state initiative)
            alive (alive-on-each-side (:combatants round-result))]
        (cond
          (<= (count alive) 1)
          (assoc round-result :alive alive)

          (> round 25)
          (-> round-result
            (update :messages conj "And then everyone gave up because their arms were tired")
            (assoc :alive alive))

          :else
          (recur (inc round) round-result initiative))))))

(defn process-encounters [peeps encounters]
  (reduce
    (fn [[prior-messages peeps] mobs]
      (let [{:keys [alive combatants messages] :as _encounter-result} (combat-encounter peeps mobs)
            peeps' (reduce
                     (fn [v {:peep/keys [name] :as peep}]
                       (conj v (get combatants name peep)))
                     []
                     peeps)
            messages' (into prior-messages messages)]
        (if (contains? alive :peep)
          [messages' peeps']
          (reduced [messages' peeps']))))
    [["Starting Adventure"] peeps]
    encounters))

(defn alive? [{hp :combat/hp :as peep}]
  (> hp 0))

(defn post-combat-peep-steps [peep {:keys [rewards]}]
  (-> peep
    (cond->
      (and rewards (alive? peep))
      (update :stuff (fnil conj []) rewards))
    ;; de-select all peeps
    (dissoc :peep/selected)))

(defn process-quest [{:keys [quests selected-quest peeps] :as state}]
  (let [quest (get quests selected-quest)
        chosen-peeps (selected-peeps peeps)
        _ (println :quest (pr-str quest))
        _ (println :peeps (pr-str peeps))
        _ (println :chosen-peeps (pr-str chosen-peeps))
        {encounters :quest/mobs} quest
        _ (println :encounters (pr-str (first encounters)))
        [messages peeps'] (process-encounters chosen-peeps encounters)
        _ (println :quest-log (pr-str messages))
        names->peeps' (into {} (map (juxt :peep/name identity)) peeps')
        _ (println :names->peeps' (pr-str names->peeps'))
        peeps' (reduce-kv
                 (fn [m peep-name peep]
                   (let [peep' (-> (get names->peeps' peep-name peep)
                                 (post-combat-peep-steps quest))]
                     (assoc m peep-name peep')))
                 {}
                 peeps)
        _ (println :peeps' (pr-str peeps'))]
    (-> state
      (update-in [:message-log :message-chunks] conj messages)
      (update-in [:message-log :size] + (count messages))
      (assoc :peeps peeps'))))

(defn make-peep [[name class]]
  {:peep/name name :peep/class class :combat/hit "2d4" :combat/dmg "2d4" :combat/def 4 :combat/hp 5 :combat/max-hp 5})

(defn make-rat [name]
  {:mob/name name :combat/hit "1d6" :combat/dmg "2d4" :combat/def 4 :combat/hp 2 :combat/max-hp 2})

(defn make-goblin [name]
  {:mob/name name :combat/hit "1d6" :combat/dmg "2d4" :combat/def 4 :combat/hp 4 :combat/max-hp 4})

(defn empty-state []
  {:player-hp 20
   :terrain (rand-tile-seqs 10)
   :typing  ""
   :units (into {} (map (juxt (juxt :x :y) identity)) (repeatedly 10 #(hash-map :x (inc (rand-int 15)) :y (inc (rand-int 10)) :glyph (rand-nth (vals units)))))
   :quests (into (sorted-map) {"Goblins Attack Farm!" {:quest/name "Goblins Attack Farm!" :quest/mobs [(mapv make-goblin ["Goblin 1" "Goblin 2" "Goblin 3"])]}
                               "Cull Local Rats!" {:quest/name "Cull Local Rats!" :quest/mobs [(mapv make-rat ["Rat 1"]) (mapv make-rat ["Rat 1" "Rat 2" "Rat 3"])]}})
   :selected-quest "Cull Local Rats!"
   ;:peep/selected false
   :peeps (into (sorted-map) (into {} (map (juxt first make-peep)) [["Peep 1" :mage] ["Peep 2" :fighter]]))
   :message-log {:size 2
                 :message-chunks [["Welcome to Fruit Economy!" "Have fun!"]]}})

(def *state (atom (empty-state)))

(comment
  (reset! *state (empty-state)))

(defn won? [{:keys [player-hp]}]
  (> player-hp 0))

(defn on-key-press [code]
  (println :code (pr-str code))
  #_
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
          (swap! *state #(-> % (assoc :typing "") (update :terrain conj typing))))))))

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
                   {:keys [terrain units] :as state} @*state]
    (let [fill (fn [tile]
                 (paint/fill (render-tile-colour tile)))
          unit-glyph (fn [_tile x y] (get-in units [[x y] :glyph] " "))]
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

#_
(defn key
  ([char] (key char {:width 25 :code char}))
  ([char {:keys [width code]}]
   (ui/clickable
     #(on-key-press code)
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
                      colour (when (= selected-quest quest-name) :green)]
       (ui/fill
         (case colour
           :green  fill-green
           :yellow fill-yellow
           :gray   fill-dark-gray
           nil     fill-light-gray)
         (ui/width width
           (ui/halign 0.5
             (ui/height 35
               (ui/valign 0.5
                 (ui/label quest-name font-small (if (some? colour) fill-white fill-black)))))))))))

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

(defn tombstone [child]
  (ui/dynamic ctx [{:keys [font-ui fill-text leading scale]} ctx]
    (let [border (doto (Paint.)
                   (.setColor (unchecked-int 0xFF000000))
                   (.setMode PaintMode/STROKE)
                   (.setStrokeWidth (* 1 scale)))]
      (ui/column
        (ui/row
          (ui/fill border
            (ui/padding 5 5
              (ui/label "💀" font-ui fill-text))))
        (ui/gap 5 0)
        child))))

(declare show-map-ui)

(defn show-val-ui [v font fill lead-col?]
  (cond
    (instance? clojure.lang.Atom v) (show-val-ui @v font fill lead-col?)
    (vector? v) ((if lead-col? ui/column ui/row)
                 (interpose (ui/gap 2 padding) (mapv #(show-val-ui % font fill lead-col?) v)))
    (map? v) (show-map-ui v font fill (not lead-col?))
    :else (ui/label (pr-str v) font fill)))

(defn show-map-ui
  ([m font fill] (show-map-ui m font fill true))
  ([m font fill lead-col?]
   ((if lead-col? ui/column ui/row)
    (for [[k v] m]
      (ui/padding 5
        ((if lead-col? ui/row ui/column)
         (ui/label (str k) font fill)
         (ui/gap 10 0)
         (show-val-ui v font fill lead-col?)))))))

(def quest-detail-ui
  (ui/dynamic ctx [{:keys [font-large font-small stroke-light-gray stroke-dark-gray fill-green fill-yellow fill-dark-gray fill-white fill-black]} ctx
                   {:keys [quests selected-quest peeps] :as state} @*state]
    (ui/column
      (show-map-ui (get quests selected-quest) font-large fill-black)
      (ui/gap 0 padding)
      (ui/halign 0.5
        (ui/row
          (interpose (ui/gap padding 2)
            (for [[name peep] peeps
                  :let [alive? (alive? peep)]]
              (ui/fill (if alive? fill-yellow fill-dark-gray)
                (ui/padding 10
                  (let [display-fn (if-not alive? tombstone (partial checkbox [:peeps name :peep/selected]))]
                    (ui/column
                      (display-fn
                        (show-map-ui peep font-small fill-black))))))))))
      (ui/gap 0 padding)
      (let [selected-peeps? (seq (selected-peeps peeps))]
        (ui/halign 0.5
          (ui/fill (if selected-peeps? fill-green fill-dark-gray)
            (ui/clickable
              #(when selected-peeps? (swap! *state process-quest))
              (ui/padding 10 10
                (ui/label "⇫ Begin" font-small fill-white)))))))))


#_
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

(defn message-log-ui
  ([] (message-log-ui nil))
  ([limit]
   (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black scale]} ctx
                    {:keys [message-log]} @*state]
     (let [{:keys [size message-chunks]} message-log
           message-log' (if (and limit (> size limit)) (subvec message-chunks (- size limit)) message-chunks)]
       (ui/column
         (ui/gap 0 padding)
         (ui/halign 0.5
           (ui/label "[ Message Log ]" font-small fill-black))
         (ui/gap 0 (* padding 2))
         (ui/halign 0.5
           (ui/halign 0.5
             (ui/column
               [:stretch 1
                (ui/column
                  (interpose (ui/gap 2 padding)
                    (map
                      (fn [message-chunk]
                        (let [border (doto (Paint.)
                                       (.setColor (unchecked-int 0xFF000000))
                                       (.setMode PaintMode/STROKE)
                                       (.setStrokeWidth (* 1 scale)))]
                          (ui/halign 0.5
                            (ui/fill border
                              (ui/padding 5 5
                                (ui/column
                                  (interpose (ui/gap 2 padding)
                                    (for [message message-chunk]
                                      (ui/halign 0.5 (ui/label (str message) font-small fill-black))))))))))
                      message-log')))]))))))))

(def map-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
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
              (ui/halign 0.5 (message-log-ui 10)))))))))

(def quest-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
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
              (ui/halign 0.5 (message-log-ui 10)))))))))

(def peep-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx
                       peeps (:peeps @*state)]
        (let [font-small (Font. ^Typeface typeface (float (* scale 13)))
              fill-black (paint/fill 0xFF000000)
              fill-yellow (paint/fill 0xFFC9B457)
              fill-light-gray (paint/fill 0xFFD4D6DA)
              fill-dark-gray (paint/fill 0xFF777C7E)]
          (ui/with-context
            {:font-large      (Font. ^Typeface typeface (float (* scale 26)))
             :font-small      font-small
             :fill-white      (paint/fill 0xFFFFFFFF)
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  fill-dark-gray
             :fill-green      (paint/fill 0xFF6AAA64)
             :fill-yellow     fill-yellow
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              (ui/halign 0.5
                (ui/row
                  (interpose (ui/gap padding 2)
                    (for [[name peep] peeps
                          :let [alive? (alive? peep)]]
                      (ui/fill (if alive? fill-yellow fill-dark-gray)
                        (ui/padding 10
                          (let [display-fn (if-not alive? tombstone (partial checkbox [:peeps name :peep/selected]))]
                            (ui/column
                              (display-fn
                                (show-map-ui peep font-small fill-black)))))))))))))))))

(def messages-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
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
              [:stretch 1
               (ui/vscrollbar
                 (ui/vscroll
                   (ui/column
                     (message-log-ui))))])))))))

(def ui-views
  ;; exploiting the fact that as long as array-map doesn't grow, it keeps insertion order
  (array-map
    "Map" map-ui-view
    "Quest" quest-ui-view
    "Peep" peep-ui-view
    "Messages" messages-ui-view))

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
