(ns fe-7drl-2022.map
  (:require [fe-7drl-2022.noise :refer [coord->noise simplex-noise-fn]]))


(defn simplex-noise-map [width height {:keys [seed octave scale low high] :or {scale 1 low 0 high 1}}]
  (let [norm (fn [v size] (/ (float v) size))]
    (into []
      (for [y (range height)]
        (into []
          (for [x (range width)]
            (coord->noise simplex-noise-fn [(norm x width) (norm y height)]
              {:seed seed :octave octave :lacunarity 1
               :persistence 0.5 :scale [scale scale]
               :normalise? true :low -1 :high 1})))))))

(defn make-noise-map [width height seed-size octaves]
  (let [[o0 o1 o2] octaves]
    {:width width :height height
     :noise-coll
     [(simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o0})
      (simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o1})
      (simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o2})]}))

(defn make-temp-noise-map [width height]
  (make-noise-map width height 10000 [2 4 88]))

(defn make-elev-noise-map [width height]
  (make-noise-map width height 10000 [4 10 20]))

(defn messy-noise [noise-coll [x y]]
  (let [[n0 n1 n2] noise-coll]
    (+
      (get-in n0 [y x])
      (* (get-in n1 [y x]) 0.5)
      (* (get-in n2 [y x]) 0.25))))

(defn process-noise-map [{:keys [width height noise-coll] :as _noise-map} val-mod]
  (into []
    (map (fn [row]
           (mapv (fn [coord]
                   (+ (messy-noise noise-coll coord) val-mod))
             row)))
    (for [y (range height)]
      (into []
        (for [x (range width)]
          [x y])))))

(defn gen-land [{:keys [width height sea-level] :as land-data}]
  (reduce
    (fn [land y]
      (reduce
        (fn [{:keys [temp elev] :as land} x]
          (let [local-temp (get-in temp [y x])
                local-elev (get-in elev [y x])]
            (cond
              (and (>= local-elev sea-level) (< local-elev 0.1))
              (assoc-in land [:terrain y x] :beach)

              (and (>= local-elev 0.4) (< local-temp -0.2))
              (assoc-in land [:terrain y x] :snow-mountain)

              (>= local-elev 0.4)
              (assoc-in land [:terrain y x] :mountain)

              ;; temperate region
              (> local-elev sea-level)
              (cond
                (< local-temp -0.2)
                (assoc-in land [:terrain y x] :snow)

                (< local-temp 0)
                (assoc-in land [:terrain y x] :tundra)

                (< local-temp 0.1)
                (assoc-in land [:terrain y x] :grassland)

                (< local-temp 0.2)
                (assoc-in land [:terrain y x] :forest)

                (< local-temp 0.3)
                (assoc-in land [:terrain y x] :jungle)

                :else
                (assoc-in land [:terrain y x] :desert))

              :else
              land)))
        land
        (range width)))
    land-data
    (range height)))

(defn gen-world [width height {:keys [temp-mod elev-mod temp-noise elev-noise base-biome] :or {temp-mod 0.1 elev-mod 0 base-biome :ocean}}]
  (let [temp-noise (or temp-noise (make-temp-noise-map width height))
        elev-noise (or elev-noise (make-elev-noise-map width height))]
    (-> {:width width :height height
         :temp-noise temp-noise
         :elev-noise elev-noise
         :temp (process-noise-map temp-noise temp-mod)
         :elev (process-noise-map elev-noise elev-mod)
         :sea-level (rand-nth (range 0.001 0.007 0.001))
         :terrain (vec (repeat height (vec (repeat width base-biome))))}
      (gen-land))))
