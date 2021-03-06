(ns fe-7drl-2022.core
  (:require
   [clojure.string :as str]
   [clojure.data.finger-tree :refer [double-list conjl]]
   [environ.core :refer [env]]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [fe-7drl-2022.humble-ui :as c-hui]
   [fe-7drl-2022.components :as cui]
   [fe-7drl-2022.lists :refer [adjectives]]
   [fe-7drl-2022.map :refer [gen-world]])
  (:import
   [io.github.humbleui.jwm App EventFrame EventMouseButton EventMouseMove EventMouseScroll EventKey Window]
   [io.github.humbleui.skija Canvas FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint])
  (:gen-class))


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

(def resources
  {:growing-plant "🌱" :dying-plant "🥀"
   :bush "🌳" :tree-1 "🌴" :tree-2 "🌲" :tree-3 "🌵" :tree-4 "🌾" :tree-5 "🎋" :tree-6 "🎍" :magic-tree "🎄"
   :flower-1 "🌸" :flower-2 "💮" :flower-3 "🏵️" :flower-4 "🌺" :flower-5 "🌻" :flower-6 "🌼" :flower-7 "🌷"
   :herb-1 "🌿" :herb-2 "☘️" :herb-3 "🍀" :herb-4 "🍁" :shroom "🍄" :nut-1 "🌰" :nut-2 "🥥"
   :fruit-1 "🍇" :fruit-2 "🍈" :fruit-3 "🍉" :fruit-4 "🍊" :fruit-5 "🍋" :fruit-6 "🍌" :fruit-7 "🍍" :fruit-8 "🥭"
   :fruit-9 "🍎" :fruit-10 "🍏" :fruit-11 "🍐" :fruit-12 "🍑" :fruit-13 "🍒" :fruit-14 "🍓" :fruit-15 "🥝" :fruit-16 "🍅"})

(def units
  {:rabbit "🐇" :deer "🦌" :dragon "🐉" :spider "🕷️" :rat "🐀" :bear "🐻" :snake "🐍" :frog "🐸"})

(def stats
  {:rabbit {:dmg "2d3-2" :def 10 :hp 3 :special {:stealth "1d20+12"}}
   :deer {:dmg "2d6+4" :def 12 :hp 12}
   :dragon {:dmg "3d6+3" :def 18 :hp 20}
   :spider {:dmg "3d3-1" :def 12 :hp 4 :special {:stealth "1d20+10"}}
   :rat {:dmg "2d3-4" :def 14 :hp 2 :special {:stealth "1d20+16"}}
   :bear {:dmg "2d6+2" :def 14 :hp 15}
   :snake {:dmg "1d6+3" :def 12 :hp 7 :special {:stealth "1d20+6"}}
   :frog {:dmg "1d6+2" :def 14 :hp 5 :special {:stealth "1d20+4"}}})

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
        [_ n d _ op-name mod] (re-matches #"(\d+)d(\d+)((\+|\-)(\d+))?" dice)
        [n d] [(str->int n) (str->int d)]
        op (condp = op-name
             "+" +
             "-" -
             identity)]
    (cond->
      (vec (basic-roll n d))
      op-name
      (conj (partial op (str->int mod))))))

(defn roll->result [dice-seq]
  (let [calc (fn [acc op-or-val]
               (if (fn? op-or-val)
                 (op-or-val acc)
                 (+ acc op-or-val)))]
    (reduce calc dice-seq)))

(comment
  (= (roll "1d1") [1])
  (= (roll "2d1") [1 1])
  (= (roll->result (roll "1d1+1")) 2)
  (= (roll->result (roll "2d1+1")) 3))

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
            hit-roll (roll->result (roll (:combat/hit atker)))
            target-def (:combat/def target)
            hits? (> hit-roll target-def)
            dmg-roll (roll->result (roll (:combat/dmg atker)))
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

(defn roll-initiative [combatants-coll] (mapv second (sort-by first (mapv (fn [m] [(roll->result (roll (:combat/init m))) m]) combatants-coll))))

(defn combat-encounter [peeps mobs]
  (let [init-initiative (roll-initiative (into peeps mobs))
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
          [messages' peeps' alive]
          (reduced [messages' peeps' alive]))))
    [["Starting Adventure"] peeps]
    encounters))

(defn alive? [{hp :combat/hp :as peep}]
  (> hp 0))

(defn quest-success? [{peep :peep :as alive}]
  peep)

(defn post-combat-peep-steps [peep {:quest/keys [rewards]}]
  (-> peep
    (cond->
      (and rewards (alive? peep))
      (update :stuff (partial merge-with +) rewards))
    ;; de-select all peeps
    (dissoc :peep/selected)))

(defn process-quest-consequences [state {:quest/keys [success failure] :as quest} success?]
  (reduce-kv
    (fn [m k v]
      (let [op (condp = v
                 :inc inc
                 :dec dec)]
        (update-in m k op)))
    state
    (if success? success failure)))

(defn refresh-quests [all-quests]
  (into (sorted-map) (map (juxt :quest/name identity)) (take 4 (shuffle all-quests))))

(defn process-quest [{:keys [quests selected-quest peeps all-quests] :as state}]
  (let [quest (get quests selected-quest)
        chosen-peeps (selected-peeps peeps)
        _ (println :quest (pr-str quest))
        _ (println :peeps (pr-str peeps))
        _ (println :chosen-peeps (pr-str chosen-peeps))
        {encounters :quest/mobs} quest
        _ (println :encounters (pr-str (first encounters)))
        [messages peeps' alive] (process-encounters chosen-peeps encounters)
        _ (println :quest-log (pr-str messages))
        _ (println :alive (pr-str alive))
        names->peeps' (into {} (map (juxt :peep/name identity)) peeps')
        _ (println :names->peeps' (pr-str names->peeps'))
        peeps' (reduce-kv
                 (fn [m peep-name peep]
                   (->>
                     (if-let [peep' (get names->peeps' peep-name)]
                       (post-combat-peep-steps peep' quest)
                       peep)
                     (assoc m peep-name)))
                 {}
                 peeps)
        _ (println :peeps' (pr-str peeps'))
        dead-peeps (into [] (remove alive?) (vals names->peeps'))
        _ (println :dead-peeps (pr-str dead-peeps))
        new-quests (refresh-quests all-quests)]
    (-> state
      (update-in [:message-log :message-chunks] conjl messages)
      (update-in [:message-log :size] + (count messages))
      (assoc :peeps peeps')
      (update :tick inc)
      (assoc :power 2)
      (update :buildings #(into {} (map (fn [[k v]] [k (dissoc v :building/used)])) %))
      (update :player-hp #(max (- % (count dead-peeps)) 0))
      (assoc :quests new-quests :selected-quest (ffirst new-quests))
      (process-quest-consequences quest (quest-success? alive)))))

(defn make-peep [[name class]]
  {:peep/name name :peep/class class :combat/init "1d20+3" :combat/hit "1d20+4" :combat/dmg "2d4" :combat/def 14 :combat/hp 12 :combat/max-hp 12})

(defn make-rat [name]
  {:mob/name name :combat/stealth "1d20+16" :combat/init "1d20+2" :combat/hit "1d20+4" :combat/dmg "1d3-4" :combat/def 14 :combat/hp 1 :combat/max-hp 1})

(defn make-goblin [name]
  {:mob/name name :combat/stealth "1d20+6" :combat/init "1d20+2" :combat/hit "1d20+4" :combat/dmg "1d6+2" :combat/def 15 :combat/hp 7 :combat/max-hp 7})

(defn make-kin
  ([dmg def hp] (make-kin dmg def hp {}))
  ([dmg def hp {:keys [stealth]}]
   (fn [name]
     (merge {:mob/name name :combat/init "1d20+2" :combat/hit "1d20+4" :combat/dmg dmg :combat/def def :combat/hp hp :combat/max-hp hp}
       (when stealth
         {:combat/stealth stealth})))))

(defn make-building [[name class]]
  {:building/name name :building/class class})

(defn used? [{used :building/used :as _building}]
  used)

(defn process-building-activation [{:keys [selected-building buildings peeps] :as state}]
  (let [activated-building (get buildings selected-building)
        peep-name (str "Peep " (rand-int 10000))
        new-peep (make-peep [peep-name (:building/class activated-building)])]
    (println activated-building new-peep)
    (-> state
      (assoc-in [:buildings selected-building :building/used] true)
      (update :power dec)
      (assoc-in [:peeps peep-name] new-peep)
      (dissoc :selected-building))))

(defn spawn-player [width height terrain units]
  (let [player-location
        (loop [x (rand-int width) y (rand-int height)
               attempt 0]
          (cond
            (> attempt 100)
            (ffirst units) ;; we just take over an existing space

            (and
              (not= (get-in terrain [y x]) :ocean)
              (contains? units [x y]))
            [x y]

            :else
            (recur
              (rand-int width)
              (rand-int height)
              (inc attempt))))
        [x y] player-location]
    (assoc units player-location {:x x :y y :glyph "🧙" :name "You the Player" :short-name "You"})))

(defn generate-world [width height n]
  (let [{:keys [terrain]} (gen-world width height {})
        resources (into {} (comp (map (juxt (juxt :x :y) identity)) (filter (fn [[k v]] (let [[x y] k biome (get-in terrain [y x])] (when (not= biome :ocean) [k v]))))) (repeatedly n #(hash-map :x (rand-int width) :y (rand-int height) :glyph (rand-nth (vals resources)))))
        unit-names (keys units)
        adjective-names (keys adjectives)
        units (into {} (comp (map (juxt (juxt :x :y) identity)) (filter (fn [[k v]] (let [[x y] k biome (get-in terrain [y x])] (when (not= biome :ocean) [k v]))))) (repeatedly n #(let [kind (rand-nth unit-names) adjective (rand-nth adjective-names) glyph (get units kind)] (merge {:x (rand-int width) :y (rand-int height) :name (str (name adjective) " " (name kind) "-kin") :short-name (str (get adjectives adjective) (get units kind)) :glyph glyph} (get stats kind)))))
        with-player (spawn-player width height terrain units)]
    (println (count units) :units units)
    {:terrain terrain
     :units with-player
     :resources resources}))

(defn val->rel-glyph [v]
  (condp = v
    -3 "☠️"
    -2 "💩"
    -1 "🤡"
    0 "🤷"
    1 "👍"
    2 "🤝"
    3 "❤️"
    "🤷"))

(defn gen-relations [units]
  (let [locations (keys units)]
    (into {}
      (comp
        (map
          (fn [[location {:keys [name] :as tribe}]]
            {name
             (apply merge
               (let [gregariousness (rand-nth [0 1 1 1 2 2 2 2 2 3 3 3 3 4 4 5 5 6])]
                 (for [_ (range gregariousness)
                       :let [loc (rand-nth locations)]
                       :when (not= loc location)]
                   {(:name (get units loc))
                    (rand-nth [-3 -2 -1 0 1 2 3])})))}))
        (remove (fn [v] (nil? (first (vals v))))))
      units)))

(defn gen-inter-relations [relations]
  (reduce-kv
    (fn [relate our-name relation-map]
      (merge relate
        (reduce-kv
          (fn [relate their-name relation-val]
            (println our-name their-name [relation-val (get (get relations their-name) our-name)])
            (assoc relate [our-name their-name] [relation-val (get (get relations their-name) our-name)]))
          relate
          relation-map)))
    {}
    relations))

(defn make-quest [kin-name kin-short-name make-kin]
  (let [quest-name (str "Attack " kin-name "s!")
        quest-structure (rand-nth
                          [[(mapv make-kin [(str kin-name " 1")]) (mapv make-kin [(str kin-name " 1") (str kin-name " 2") (str kin-name " 3")])]
                           [(mapv make-kin [(str kin-name " 1") (str kin-name " 2") (str kin-name " 3")])]])]
    {:quest/name quest-name :quest/mobs quest-structure :quest/rewards {:copper (roll->result (roll "2d6+2"))} :quest/success {[:reputation kin-short-name] :inc} :quest/failure {[:reputation kin-short-name] :dec}}))

(defn try-generate-world [width height n]
  (loop [{:keys [terrain units resources]} (generate-world width height n)
         attempt 0]
    (println (count units) (< 6 (count units) 12) attempt (> attempt 100))
    (if (or (< 6 (count units) 12) (> attempt 100))
      (let [relations (gen-relations units)
            inter-relations (gen-inter-relations relations)
            unit-seq (vals units)
            reputation (into {} (map (fn [v] [(:short-name v) 5])) unit-seq)
            all-quests (into [] (comp (filter #(not= (:short-name %) "You")) (map (fn [{:keys [name short-name dmg def hp special]}] (make-quest name short-name (make-kin dmg def hp special))))) unit-seq)
            quests (into (sorted-map) (map (juxt :quest/name identity)) (take 4 (shuffle all-quests)))]
        (println reputation)
        {:terrain terrain
         :loc->unit (into {} (map (fn [[k v]] [(:name v) k])) units)
         :name->short-name (into {} (map (fn [[k v]] [(:name v) (:short-name v)])) units)
         :units (merge units resources)
         :relations relations
         :inter-relations inter-relations
         :reputation reputation
         :all-quests all-quests
         :quests quests
         :selected-quest (ffirst quests)})
      (recur
        (generate-world width height n)
        (inc attempt)))))

(defn empty-state []
  (->
    {:player-hp 10
     :tick 0
     :power 2
     :typing  ""
     ;:peep/selected false
     :peeps (into (sorted-map) (into {} (map (juxt first make-peep)) [["Peep 1" (rand-nth [:mage :rogue :fighter :cleric])] ["Peep 2" (rand-nth [:mage :rogue :fighter :cleric])]]))
     :buildings (into (sorted-map) (into {} (map (juxt first make-building)) [["Mage Building" :mage] ["Rogue Building" :rogue] ["Fighter Building" :fighter] ["Cleric Building" :cleric]]))
     :message-log {:size 2
                   :message-chunks (double-list ["Welcome to Fruit Economy!" "Have fun!"])}}
    (merge (try-generate-world 30 20 30))))

(def *state (atom (empty-state)))

(comment
  (reset! *state (empty-state)))

(defn won? [{:keys [player-hp]}]
  (> player-hp 0))

(defn lost? [{:keys [player-hp]}]
  (<= player-hp 0))

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
        (interpose (ui/gap 0 1)
          (for [[tile-row y-idx] (map vector terrain (range))]
            (ui/row
              (interpose (ui/gap 1 0)
                (for [[tile x-idx] (map vector tile-row (range))]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [_ (when hovered?
                                (swap! *state assoc :hover-loc [x-idx y-idx]))]
                        (ui/fill (if hovered?
                                   (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                   (fill tile))
                          (ui/width 25
                            (ui/halign 0.5
                              (ui/height 25
                                (ui/valign 0.5
                                  (ui/label (unit-glyph tile x-idx y-idx) font-large fill-white))))))))))))))
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
         (ui/gap 10 5)
         (show-val-ui v font fill lead-col?)))))))

(def quest-detail-ui
  (ui/dynamic ctx [{:keys [font-large font-small stroke-light-gray stroke-dark-gray fill-green fill-yellow fill-dark-gray fill-white fill-black]} ctx
                   {:keys [quests selected-quest peeps] :as state} @*state]
    (ui/column
      (show-map-ui (get quests selected-quest) font-small fill-black)
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

(defn nested-limit
  ([coll limit] (nested-limit coll limit nil))
  ([coll limit elide-str]
   (let [cond-add-elide (fn [v] (if elide-str (conj v elide-str) v))]
     (reduce
       (fn [[rem v] item]
         (let [size (count item)
               rem' (- rem size)]
           (cond
             (< rem size) (reduced (conj v (cond-add-elide (into [] (take rem) item))))
             (> rem' 0) [rem' (conj v item)]
             (zero? rem') (reduced (conj v item)))))
       [limit []]
       coll))))

(comment
  ;; Can turn to tests later...
  (let [;; 3 => [[1 2] [3]]
        ;; 4 => [[1 2] [3 4]]
        coll [[1 2] [3 4] [5 6]]
        coll' [[1 2] [3 4 5 6 7 8 9 10]]]
    (and
      (= [[1 2] [3]]
         (nested-limit coll 3))
      (= [[1 2] [3 4]]
         (nested-limit coll 4))
      (= [[1 2] [3 4]]
        (nested-limit coll' 4)))))

(defn message-log-ui
  ([] (message-log-ui nil))
  ([limit]
   (ui/dynamic ctx [{:keys [font-small fill-light-gray fill-black scale]} ctx
                    {:keys [message-log]} @*state]
     (let [{:keys [size message-chunks]} message-log
           message-log' (if (and limit (> size limit)) (nested-limit message-chunks limit "...") message-chunks)]
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

(def top-bar-ui
  (ui/dynamic ctx [{:keys [font-small fill-black fill-yellow fill-white scale]} ctx]
    (ui/column
      [:stretch 1
       (ui/padding 0 0 0 10
         (ui/fill fill-yellow
           (ui/row
             (ui/padding 10 10
               (ui/dynamic ctx [tick (:tick @*state)]
                 (ui/label (str "Day " (inc tick)) font-small fill-black)))
             (ui/padding 10 10
               (ui/dynamic ctx [power (:power @*state)]
                 (ui/label (str "Agents [" (apply str (repeat power "♙")) "]") font-small fill-black)))
             (ui/padding 10 10
               (ui/dynamic ctx [player-hp (:player-hp @*state)]
                 (ui/label (str "Renown " (str player-hp)) font-small fill-black)))
             (ui/dynamic ctx [reputation (:reputation @*state)]
               (let [your-rep (get reputation "You")
                     reputation (dissoc reputation "You")
                     rep-str (str/join ", " (into [] (comp (map (fn [[k v]] (str k " " v)))) reputation))]
                 (ui/padding 10 10 0 10
                   (ui/label (str "Your Rep [" your-rep "] Reputation [" rep-str "]") font-small fill-black))))
             [:stretch 1 nil]
             (ui/fill fill-white
               (ui/clickable
                 #(reset! *state (empty-state))
                 (ui/padding 10 10
                   (ui/label "↻ Restart" font-small fill-black)))))))])))

(def map-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx]
        (let [font-large (Font. ^Typeface typeface (float (* scale 26)))
              font-small (Font. ^Typeface typeface (float (* scale 13)))
              fill-black (paint/fill 0xFF000000)
              fill-light-gray (paint/fill 0xFFD4D6DA)]
          (ui/with-context
            {:font-large      font-large
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
              top-bar-ui
              (ui/halign 0.5
                (ui/dynamic ctx [_ (:selected-quest @*state)]
                  (ui/row
                    (interpose (ui/gap 2 padding)
                      (for [[name _quest] (:quests @*state)
                            :let [label-width (+ (* (count name) 8) padding)]]
                        (quest-ui-btn name {:width label-width :code name :selected-quest (:selected-quest @*state)}))))))
              (ui/gap 0 padding)
              [:stretch 1 nil]
              (ui/row
                field
                (ui/gap padding 0)
                (ui/column
                  (ui/padding 50
                    (ui/label "Info Panel" font-large fill-black))
                  (ui/padding 50
                    (ui/dynamic _ [hover-loc (:hover-loc @*state)]
                      (let [{:keys [units reputation inter-relations name->short-name]} @*state
                            tribe (get units hover-loc)
                            {:keys [name short-name]} tribe
                            rep (get reputation short-name)
                            rel (into [] (comp (filter (comp #(contains? % name) set first)) (map (fn [[k v]] [(str/join ">  <" (map (comp (partial str/join "=") vector) (mapv name->short-name k) (mapv val->rel-glyph v)))]))) inter-relations)
                            m (merge tribe
                                (when rep
                                  {:reputation rep})
                                (when (seq rel)
                                  {:relations rel}))]
                        (show-map-ui m font-small fill-black))))))
              [:stretch 1 nil]
              (ui/gap 0 padding)
              (ui/halign 0.5 (message-log-ui 6)))))))))

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
              top-bar-ui
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
              (ui/halign 0.5 (message-log-ui 5)))))))))

(def peep-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx
                       peeps (:peeps @*state)
                       buildings (:buildings @*state)
                       selected-building (:selected-building @*state)
                       power (:power @*state)]
        (let [font-small (Font. ^Typeface typeface (float (* scale 13)))
              fill-white (paint/fill 0xFFFFFFFF)
              fill-black (paint/fill 0xFF000000)
              fill-green (paint/fill 0xFF6AAA64)
              fill-yellow (paint/fill 0xFFC9B457)
              fill-light-gray (paint/fill 0xFFD4D6DA)
              fill-dark-gray (paint/fill 0xFF777C7E)]
          (ui/with-context
            {:font-large      (Font. ^Typeface typeface (float (* scale 26)))
             :font-small      font-small
             :fill-white      fill-white
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  fill-dark-gray
             :fill-green      fill-green
             :fill-yellow     fill-yellow
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              top-bar-ui
              (ui/halign 0.5
                (ui/row
                  (interpose (ui/gap padding 2)
                    (for [[name building] buildings
                          :let [used? (used? building)]]
                      (ui/fill (if used? fill-dark-gray fill-green)
                        (ui/padding 10
                          (let [display-fn (if-not used? (partial cui/radio-button *state name [:selected-building]) identity)]
                            (ui/column
                              (display-fn
                                (show-map-ui building font-small fill-black))))))))))
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
              (let [power? (> power 0)
                    active? (and selected-building power?)
                    btn-message (if power? (str "Activate " selected-building " with one Agent") (str "Your Agents are all busy, why not send some Peeps on a Quest?"))]
                (c-hui/<>
                  (when selected-building
                    (ui/gap 0 padding)
                    (ui/halign 0.5
                      (ui/padding 10 10
                        (ui/label btn-message font-small fill-black))))
                  (ui/gap 0 padding)
                  (ui/halign 0.5
                    (ui/fill (if active? fill-green fill-dark-gray)
                      (ui/clickable
                        #(when active? (swap! *state process-building-activation))
                        (ui/padding 10 10
                          (ui/label "⇫ Act" font-small fill-white))))))))))))))

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
              top-bar-ui
              [:stretch 1
               (ui/vscrollbar
                 (ui/vscroll
                   (ui/column
                     (message-log-ui))))])))))))

(def lose-ui-view
  (ui/on-key-down #(on-key-press (:hui.event.key/key %))
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx
                       peeps (:peeps @*state)]
        (let [font-large (Font. ^Typeface typeface (float (* scale 26)))
              font-small (Font. ^Typeface typeface (float (* scale 13)))
              fill-white (paint/fill 0xFFFFFFFF)
              fill-black (paint/fill 0xFF000000)
              fill-yellow (paint/fill 0xFFC9B457)
              fill-light-gray (paint/fill 0xFFD4D6DA)]
          (ui/with-context
            {:font-large      font-large
             :font-small      font-small
             :fill-white      fill-white
             :fill-black      fill-black
             :fill-light-gray fill-light-gray
             :fill-dark-gray  (paint/fill 0xFF777C7E)
             :fill-green      (paint/fill 0xFF6AAA64)
             :fill-yellow     fill-yellow
             :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
             :stroke-dark-gray  (paint/stroke 0xFF777C7E (* 2 scale))}
            (ui/column
              (ui/halign 0.5
                (ui/fill fill-white
                  (ui/clickable
                    #(reset! *state (empty-state))
                    (ui/padding 10 10
                      (ui/label "↻ Restart" font-small fill-black)))))
              [:stretch 1 nil]
              (ui/valign 0.5
                (ui/halign 0.5
                  (ui/padding 10 10
                    (ui/label "You Lose" font-large fill-black))))
              [:stretch 1 nil])))))))

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
  (ui/dynamic ctx [scale (:scale ctx)
                   player-hp (:player-hp @*state)]
    (let [font-ui   (Font. face-default (float (* 13 scale)))
          leading   (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))]
      (ui/with-context {:face-ui   face-default
                        :font-ui   font-ui
                        :leading   leading
                        :fill-text fill-text}
        (if (lost? @*state)
          lose-ui-view
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
               (ui-views name))]))))))

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
        {:on-close (if (debug?) #(reset! *window nil) #(System/exit 0))
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
