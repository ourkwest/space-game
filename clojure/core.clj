(ns core
  (:require [see :as see]
            [clojure.core.async :as async]
            [clojure.string :as string])
  (:import [java.awt.image BufferedImage]
           [java.awt Graphics2D Color BasicStroke Font RenderingHints]))


(def TAU (* Math/PI 2))

(defmacro destr [java-object & accessors]
  (let [fields (map (fn [accessor]
                      (list accessor java-object)) accessors)]
    `[~@fields]))

(defn but-nth [n coll]
  (concat (take n coll)
          (drop (inc n) coll)))

(defn keepcat [f coll]
  (apply concat (keep f coll)))

(def width 2200)
(def height 1400)
(def player-size 40)
(def bullet-size 4)

(defn increase-power [power]
  (min (inc power) 80))

(defn decrease-power [power]
  (max (dec power) -0))

(defn increase-angle [angle]
  (-> angle (/ 90) inc int (* 90)))

(defn decrease-angle [angle]
  (-> angle (/ 90) dec int (* 90)))

(defonce key-chan (async/chan (async/dropping-buffer 10)))

(defn key-handler-fn [key-code]
  (async/>!! key-chan key-code))

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce q (see/see image :key-handler-fn key-handler-fn))
(defonce g ^Graphics2D (.getGraphics image))

(.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
(.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)


(defn rgb
  ([r g b]
   (rgb r g b 255))
  ([r g b a]
   (Color. (int (min 255 (max 0 r)))
           (int (min 255 (max 0 g)))
           (int (min 255 (max 0 b)))
           (int (min 255 (max 0 a))))))

(defn rgba-lerp [c1 c2 p]
  (let [r1 (.getRed c1)
        g1 (.getGreen c1)
        b1 (.getBlue c1)
        a1 (.getAlpha c1)
        r2 (.getRed c2)
        g2 (.getGreen c2)
        b2 (.getBlue c2)
        a2 (.getAlpha c2)
        ip (- 1 p)]
    (rgb (+ (* ip r1) (* p r2))
         (+ (* ip g1) (* p g2))
         (+ (* ip b1) (* p b2))
         (+ (* ip a1) (* p a2)))))

(defn rgb-lerp [c1 c2 p]
  (let [r1 (.getRed c1)
        g1 (.getGreen c1)
        b1 (.getBlue c1)
        r2 (.getRed c2)
        g2 (.getGreen c2)
        b2 (.getBlue c2)
        ip (- 1 p)]
    (rgb (+ (* ip r1) (* p r2))
         (+ (* ip g1) (* p g2))
         (+ (* ip b1) (* p b2)))))

(defn set-alpha [c1 a]
  (rgb (.getRed c1) (.getGreen c1) (.getBlue c1) a))

(defn circle
  ([[x y] radius color] (circle x y radius color))
  ([x y radius color]
   (.setColor g color)
   (.fillArc g
             (int (- x radius))
             (int (- y radius))
             (int (* radius 2))
             (int (* radius 2))
             0 360)))

(defn outlined-circle
  ([[x y] radius color] (outlined-circle x y radius color))
  ([x y radius color]
   (.setColor g color)
   (.fillArc g
             (int (- x radius))
             (int (- y radius))
             (int (inc (* radius 2)))
             (int (inc (* radius 2))) 0 360)
   (.setColor g Color/BLACK)
   (.drawArc g
             (int (- (int x) radius))
             (int (- (int y) radius))
             (int (inc (* radius 2)))
             (int (inc (* radius 2))) 0 360)))

(defn line
  ([[x1 y1] [x2 y2] color]
   (line x1 y1 x2 y2 color))
  ([x1 y1 x2 y2 colour]
   (.setColor g colour)
   (.drawLine g (int x1) (int y1) (int x2) (int y2))))

(defn text [[xc yc] message color font-size]
  (.setColor g color)
  (.setFont g (Font. Font/SANS_SERIF Font/BOLD font-size))
  (let [[center-x center-y] (-> g .getFontMetrics (.getStringBounds message g)
                                (destr .getCenterX .getCenterY))]
    (.drawString g message (int (- xc center-x)) (int (- yc center-y)))))

(defn degrees->radians [degrees]
  (-> degrees (/ 360) (* TAU)))

(defn hit-detected-g
  ([[x1 y1] [x2 y2] distance]
   (hit-detected-g x1 y1 x2 y2 distance))
  ([x1 y1 x2 y2 distance]
   (let [a (- x1 x2)
         b (- y1 y2)
         c (Math/sqrt (+ (* a a) (* b b)))]
     (< c distance))))

(defn on-screen [[x y] width height margin]
  (and (< margin x (- width margin))
       (< margin y (- height margin))))

(defn angle-and-distance [[x1 y1] [x2 y2]]
  (let [x-diff (- x1 x2)
        y-diff (- y1 y2)
        angle (Math/atan2 y-diff x-diff)
        distance (Math/sqrt (+ (* x-diff x-diff)
                               (* y-diff y-diff)))]
    [angle (/ distance 10)]))

(defn calculate-gravity [mp size p]
  (let [[angle distance] (angle-and-distance p mp)
        force (/ size (* distance distance))
        force-x (* force (Math/cos angle))
        force-y (* force (Math/sin angle))]
    [force-x force-y]))

;;;;;;;;;;;;;;;;;;;;;;;;

(def gun-length 20)
(def player-radius 20)

(defn random-location [width height anti-targets]
  (println 'random-location [width height] (mapv :p anti-targets))
  (let [x (+ 50 (rand-int (- width 100)))
        y (+ 50 (rand-int (- height 100)))]
    (if (not-any? #(hit-detected-g x y
                                   (first (:p %)) (second (:p %))
                                   (+ (abs (:size %)) player-radius gun-length 10))
                  anti-targets)
      (doto [x y] (println))
      (recur width height anti-targets))))

(defn new-black-hole [p]
  (let [[lifecycle color] (if (< 0.75 (rand))
                            [(range -25 -2 0.4) Color/WHITE]
                            [(range 25 2 -0.3) Color/BLACK])]
    {:p         p
     :size      (first lifecycle)
     :lifecycle lifecycle
     :color     color}))

(defmulti tick :current-phase)

(defn initial-state [width height player-names player-colors]
  (let [info-height 50
        game-width width
        game-height (- height info-height)]
    {:width          game-width
     :height         game-height
     :info-height    info-height
     :players        (mapv (fn [player-name player-color]
                             {:label (subs player-name 0 (min 20 (count player-name)))
                              :p     (random-location game-width game-height []) ; TODO: anti-targets!
                              :size  (+ player-radius gun-length)
                              :score 0
                              :color (Color. (Integer/decode player-color))
                              :angle 0
                              :power 50})
                           player-names
                           player-colors)
     :bg-color       Color/GRAY
     :current-player 0
     :current-phase  :aiming
     :black-holes    [(new-black-hole [(* game-width 1/2) (* game-height 1/2)])]}
    {:width         game-width
     :height        game-height
     :info-height   info-height
     :players       [{:label "Start game" :kind :start}
                     {:label "Add a new player" :kind :add}
                     {:label "You" :kind :human}
                     {:label "Me" :kind :ai}]
     :current-player 0
     :bg-color      Color/GRAY
     :current-phase :setup}))

(def colors
  (->> (for [h (range 0.0 2.9 0.3)]
         (Color/getHSBColor h 1.0 1.0))
       (map (fn [^Color c]
              (let [light (/ (+ (.getRed c) (.getGreen c) (.getBlue c))
                             (+ 255 255 255))]
                (rgb-lerp c Color/WHITE (* (- 1 light) 3/5)))))
       (reverse)))

(def weapons
  [{:id    :big-gun
    :label "The Big One"
    :color (Color. (Integer/decode "#FF0000"))}
   {:id    :shot-gun
    :label "Shotgun"
    :color (Color. (Integer/decode "#1100FF"))}])

(defn initialize-players [players game-width game-height]
  (->> players
       (filter (comp #{:human :ai} :kind))
       (mapv #(assoc % :size player-radius
                       :score 0
                       :health 100
                       :current-weapon 0))
       (reduce
         (fn [output-players input-player]
           (conj output-players
                 (assoc input-player :p (random-location game-width game-height output-players))))
         [])
       (mapv (fn [color {:keys [p] :as player}]
               (assoc player :color color
                             :angle (first (angle-and-distance p [(/ game-width 2) (/ game-height 2)]))
                             :power 50))
             colors)))

(defn get-current-player [{:keys [players current-player] :as state}]
  (when (nil? current-player)
    (println state))
  (get players current-player))

(defn get-current-weapon [state]
  (when (nil? (:current-weapon (get-current-player state)))
    (println (:current-weapon (get-current-player state)))
    (println (get-current-player state))
    (println (:current-player state))
    (println (:players state))
    )
  (nth weapons (:current-weapon (get-current-player state))))

(defn await-entry [{:keys [current-player players width height] :as state}]
  (if-let [key-code (async/<!! key-chan)]
    (do
      (println key-code)
      (case key-code
        ;37 (update-in state [:players current-player :angle] dec)
        38 (assoc state :current-player (mod (dec current-player) (count players)))
        ;33 (update-in state [:players current-player :angle] increase-angle)
        ;34 (update-in state [:players current-player :angle] decrease-angle)
        ;39 (update-in state [:players current-player :angle] inc)
        40 (assoc state :current-player (mod (inc current-player) (count players)))
        8 (if (= :human (get-in state [:players current-player :kind]))
            (update-in state [:players current-player :label] (fn [x] (subs x 0 (max 0 (dec (count x))))))
            state)
        10 (do
             (println current-player ": " (get-in state [:players current-player]))
             (case (get-in state [:players current-player :kind])
             :human (assoc-in state [:players current-player :kind] :ai)
             :ai (assoc-in state [:players current-player :kind] :human)
             :add (update state :players conj {:label "Also Me" :kind :ai})
             :start (-> state
                        (update :players initialize-players width height)
                        (assoc :black-holes [(new-black-hole [(* width 1/2) (* height 1/2)])]
                               :current-phase :aiming))))
        127 (if (#{:human :ai} (get-in state [:players current-player :kind]))
              (-> state
                  (update :players (comp vec (partial but-nth current-player)))
                  (assoc :current-player 0))
              state)
        ;27 (assoc state :current-phase :exit!)
        81 (assoc state :current-phase :exit!)
        (if (or (<= 65 key-code 90)
                (= key-code 32))
          (if (= :human (get-in state [:players current-player :kind]))
            (update-in state [:players current-player :label] (fn [x]
                                                                (some-> (str x (char key-code)) string/capitalize)))
            state)
          (do
            (println key-code)
            state))))
    (assoc state :current-phase :exit!)))

(defmethod tick :setup [{:keys [players current-player width height info-height] :as state}]
  (.setColor g Color/WHITE)
  (.fillRect g 0 0 width (+ height info-height))
  (.setFont g (Font. Font/SANS_SERIF Font/BOLD 40))

  (doseq [[idx {:keys [label kind]}] (map-indexed vector players)]
    (let [selected? (= idx current-player)
          text (if (= :ai kind) (str label " (AI)") label)
          xc (/ width 2)
          yc (+ 100 (* (/ (- height 200) (count players)) idx))
          [center-x center-y label-w label-h] (-> g .getFontMetrics (.getStringBounds text g)
                                                  (destr .getCenterX .getCenterY .getWidth .getHeight))]
      (when selected?
        (.setColor g (Color. (Integer/decode "#58FF58")))
        (.fillRoundRect g
                        (int (- xc center-x 20))
                        (int (- yc center-y 10))
                        (int (+ label-w 40))
                        (int (+ label-h 40))
                        40 40)
        (.setColor g (Color. (Integer/decode "#000000")))
        (.drawString g
                     (str "▲ ▼ ⏎" (when (= kind :human) " [a-z] ⌫ ⌦"))
                     (int (+ xc center-x 50))
                     (int (- yc center-y (- label-h)))))

      (.setColor g Color/BLACK)
      (.drawString g text
                   (int (- xc center-x))
                   (int (- yc center-y (- label-h))))))
  (q)
  (await-entry state))

(defn render-players [{:keys [players current-player bg-color current-phase]}]
  (doseq [[idx {:keys [p color angle power current-weapon health]}] (map-indexed vector players)]
    (let [pf (+ player-radius (* gun-length (/ power 80)))
          aiming? (and (= current-phase :aiming) (= idx current-player))
          weapon (nth weapons current-weapon)]
      (if-not (pos? health)
        (do (circle p (+ player-radius 3) Color/DARK_GRAY))
        (do
          (when aiming?
            (circle p (inc (inc (+ player-radius gun-length))) bg-color)
            (.setStroke g (BasicStroke. 1))
            (circle p (+ player-radius gun-length) (rgba-lerp Color/WHITE bg-color 0.75))
            (outlined-circle p pf (rgba-lerp (:color weapon) bg-color 0.75)))
          (.setStroke g (BasicStroke. 10 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
          (line (first p)
                (second p)
                (+ (first p) (* (+ player-radius gun-length) (Math/sin (degrees->radians angle))))
                (+ (second p) (* (+ player-radius gun-length) (Math/cos (degrees->radians angle)) -1))
                Color/DARK_GRAY)
          (when aiming?
            (.setStroke g (BasicStroke. 6 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
            (line (first p)
                  (second p)
                  (+ (first p) (* pf (Math/sin (degrees->radians angle))))
                  (+ (second p) (* pf (Math/cos (degrees->radians angle)) -1))
                  (Color. 255 (int (+ 95 (* power 2))) 0)))

          (.setStroke g (BasicStroke. 3))
          (outlined-circle p player-radius color)

          (when aiming?
            (.setColor g Color/BLACK)
            (.setFont g (Font. Font/SANS_SERIF Font/BOLD 24))
            (let [bounds (-> g .getFontMetrics (.getStringBounds (str power) g))
                  [x y] p]
              (.drawString g (str power)
                           (int (- x (.getCenterX bounds)))
                           (int (inc (- y (.getCenterY bounds))))))))))))

(defn render-black-holes [{:keys [black-holes]}]
  (doseq [{:keys [p size]} black-holes]
    (let [[x y] p]
      (circle x y (abs size) (if (pos? size)
                               Color/BLACK
                               Color/WHITE)))))

(defn render-info-bar [{:keys [players width height info-height] :as state}]
  (.setFont g (Font. Font/SANS_SERIF Font/BOLD 24))
  (let [weapon-section 500
        player-width (/ (- width weapon-section) (count players))
        weapon (get-current-weapon state)]
    (.setColor g Color/DARK_GRAY)
    (.fillRect g 0 height width info-height)
    (.setColor g Color/BLACK)
    (.fillRect g (- width weapon-section) height weapon-section info-height)
    (.setColor g Color/WHITE)
    (text [(- width (* weapon-section 1/2))
           (int (+ height (* info-height 1/2)))]
          (str "n / p : " (:label weapon))
          Color/WHITE 40)

    (doseq [[idx {:keys [score color label health]}] (reverse (map-indexed vector players))]
      (let [corner-diameter 40
            h-inset 15
            xc (* player-width (+ idx 1/2))
            yc (+ height (/ info-height 2))
            text (str label " (" (int health) ") : " score) ; todo: render health properly! maybe on the player as well?
            [width center-x center-y] (-> g .getFontMetrics (.getStringBounds text g)
                                          (destr .getWidth .getCenterX .getCenterY))
            label-width (+ width h-inset h-inset)
            label-height info-height
            left (- xc (/ label-width 2))
            top (- yc (/ label-height 2))]
        (.setColor g color)
        (.fillRoundRect g (- left player-width) top (+ label-width player-width) label-height corner-diameter corner-diameter)
        (.setStroke g (BasicStroke. 1))
        (.setColor g Color/BLACK)
        (.drawRoundRect g (- left player-width) top (+ label-width player-width) label-height corner-diameter corner-diameter)
        (.drawString g text
                     (int (- xc center-x))
                     (int (- yc center-y))))))
  state)

(defn await-command [{:keys [players current-player] :as state}]
  (let [{:keys [kind p]} (nth players current-player)]
    (if (= kind :ai)
      (let [target (rand-nth (but-nth current-player players))]
        (Thread/sleep 1000)
        (-> state
            (assoc-in [:players current-player :angle] (-> (first (angle-and-distance p (:p target)))
                                                           (/ TAU) (* 360) (- 90) (+ (rand-int 11) -5)))
            (assoc-in [:players current-player :power] (+ 5 (rand-int 75)))
            (assoc :current-phase :firing)))
      (if-let [key-code (async/<!! key-chan)]
        (case key-code
          37 (update-in state [:players current-player :angle] dec)
          38 (update-in state [:players current-player :power] increase-power)
          33 (update-in state [:players current-player :angle] increase-angle)
          34 (update-in state [:players current-player :angle] decrease-angle)
          39 (update-in state [:players current-player :angle] inc)
          40 (update-in state [:players current-player :power] decrease-power)
          44 #_78 (update-in state [:players current-player :current-weapon] (fn [x] (mod (inc x) (count weapons))))
          46 #_80 (update-in state [:players current-player :current-weapon] (fn [x] (mod (dec x) (count weapons))))
          10 (assoc state :current-phase :firing)
          ;27 (assoc state :current-phase :exit!)
          81 (assoc state :current-phase :exit!)
          (do (println key-code)
              state))
        (assoc state :current-phase :exit!)))))

(defmethod tick :aiming [state]
  (render-players state)
  (render-black-holes state)
  (render-info-bar state)
  (q)
  (await-command state))

(defn clear [{:keys [bg-color width height info-height]}]
  (.setColor g bg-color)
  (.fillRect g 0 0 width (+ height info-height))
  (q))

(def bullet-power-factor 0.2)

(defn projectile-starting-point [p angle]
  (map +
       p
       [(* (+ player-radius gun-length) (Math/sin (degrees->radians angle)))
        (* (+ player-radius gun-length) (Math/cos (degrees->radians angle)) -1)]))

(defn projectile-starting-speed [power angle]
  [(* power (Math/sin (degrees->radians angle)) bullet-power-factor)
   (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)])

(defn gravity-force [p other-points]
  (reduce (fn [acc point]
            (map - acc (calculate-gravity (:p point) (:size point) p)))
          [0 0]
          other-points))

(defmulti process-effect :effect-type)

(defn new-sparkle
  ([p size]
   {:effect-type :sparkle :p p :size size})
  ([p offset size]
   (let [angle (rand TAU)
         r (rand offset)
         v [(* r (Math/sin angle)) (* r (Math/cos angle))]]
     (new-sparkle (map + p v) size))))

(defn new-boom [p delay offset size]
  (let [angle (rand TAU)
        r (rand offset)
        v [(* r (Math/sin angle)) (* r (Math/cos angle))]
        color (rand-nth [Color/WHITE Color/RED Color/YELLOW Color/LIGHT_GRAY Color/ORANGE])]
    {:effect-type :boom
     :p           (map + p v)
     :lifecycle   (concat (repeat delay {:size 0 :color (Color. 0 0 0 0)})
                          (map (fn [s]
                                 {:size  s
                                  :color (rgba-lerp color (set-alpha color 0) (/ s size))})
                               (range size)))}))

(defmethod process-effect :sparkle [{:keys [p size] :as sparkle}]
  (circle p size Color/WHITE)
  (when (pos? size)
    (update sparkle :size - 5)))

(defmethod process-effect :boom [{:keys [p lifecycle] :as sparkle}]
  (when (seq lifecycle)
    (let [{:keys [size color]} (first lifecycle)]
      (circle p size color))
    (update sparkle :lifecycle rest)))

(defn process-effects [effects]
  (doall (keep process-effect effects)))

(defmulti weapon-firing (comp :id get-current-weapon))
(defmulti weapon-projecting (comp :id get-current-weapon))

(defmethod weapon-firing :shot-gun [{:keys [bg-color] :as state}]
  (let [{:keys [angle power color p]} (get-current-player state)
        weapon (get-current-weapon state)
        spread-f 0.07
        spread-fn (fn [x] (* x (+ (- 1 spread-f) (rand (* 2 spread-f)))))]
    (assoc state
      :current-phase :projecting
      :projection {:projectiles (repeatedly 10 (fn []
                                                 {:color          (:color weapon)
                                                  :size           3
                                                  :damage         10
                                                  :fuse           (+ 500 (rand-int 10))
                                                  :p              (projectile-starting-point p angle)
                                                  :dp             (map spread-fn (projectile-starting-speed power angle))
                                                  :trail-config   (map (fn [p]
                                                                         {:color (rgb-lerp color bg-color p)
                                                                          :width 1})
                                                                       (range 0 1.0001 1/15))
                                                  :trail-ps       []
                                                  :expire-effects (fn [p]
                                                                    [(new-sparkle p 10)])
                                                  :hit-effects    (fn [p]
                                                                    [(new-sparkle p 20)])}))
                   :effects     [(new-sparkle (projectile-starting-point p angle) 15)]
                   :limit       20})))

(defn fall [masses]
  (fn [{:keys [p dp fuse expire-effects] :as projectile}]
    (if (zero? fuse)
      (dissoc projectile :effects)
      (let [ddp (gravity-force p masses)
            dp' (map + dp ddp)
            p' (map + p dp')
            fuse' (dec fuse)]
        (if (pos? fuse')
          (assoc projectile :p p' :dp dp' :fuse fuse')
          (assoc projectile :fuse 0
                            :effects (expire-effects p')))))))

(defn trail [{:keys [p trail-ps trail-config fuse] :as projectile}]
  (assoc projectile :trail-ps (take (count trail-config) (conj trail-ps (if (zero? fuse) nil p)))))

(defn hit [players]
  (fn [{:keys [hit-player? p damage hit-effects] :as projectile}]
    (if hit-player?
      (dissoc projectile :player-damage)
      (if-let [hit-player (first
                            (keep-indexed (fn [idx player]
                                            (when (hit-detected-g (:p player) (:p projectile)
                                                                  (+ (:size player) (:size projectile)))
                                              idx))
                                          players))]
        (assoc projectile :player-damage [hit-player damage]
                          :hit-player? true
                          :fuse 0
                          :effects (hit-effects p))
        projectile))))

(defn defuse [{:keys [width height]}]
  (fn [{:keys [p] :as projectile}]
    (if (hit-detected-g p [(/ width 2) (/ height 2)] (max width height))
      projectile
      (assoc projectile :fuse 0))))

(defn on-screen? [{:keys [width height]} {:keys [p size trail-ps]}]
  (and (< size (first p) (- width size))
       (< size (second p) (- height size))
       (if-let [last-trail-p (last trail-ps)]
         (and (< size (first last-trail-p) (- width size))
              (< size (second last-trail-p) (- height size)))
         true)))

(defn damage-players [players projectiles]
  (reduce (fn [players [idx damage]]
            (let [{:keys [health p]} (nth players idx)
                  health' (- health damage)
                  effects (when-not (pos? health')
                            (map #(new-boom p % 20 100) (range 40)))]
              (update players idx assoc :health health'
                      :effects effects
                      )))
          players
          (keep :player-damage projectiles)))

(defmethod weapon-projecting :shot-gun [{:keys [projection black-holes bg-color players] :as state}]
  (let [{:keys [projectiles limit effects]} projection
        projectiles' (map (comp (defuse state)
                                (hit players)
                                trail
                                (fall black-holes))
                          projectiles)
        effects (concat effects (keepcat :effects projectiles'))
        all-done? (every? #(-> % :fuse zero?) projectiles')
        limit' (if (and all-done? (empty? effects)) (dec limit) 20)]

    (.setColor g bg-color)
    (.fillRect g 0 0 (:width state) (+ (:height state) (:info-height state)))

    (render-players state)
    (render-black-holes state)

    (doseq [{:keys [p size color fuse trail-ps trail-config]} projectiles']
      (doseq [[{:keys [width color]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail-ps))]
        (when (and x1 x2)
          (.setStroke g (BasicStroke. width))
          (line x1 y1 x2 y2 color)))
      (when-not (zero? fuse)
        (circle p size color)))

    (when (every? (partial on-screen? state) projectiles')
      (Thread/sleep 10))

    (if (pos? limit')
      (let [players' (damage-players players projectiles')
            player-effects (keepcat :effects players')
            players'' (mapv #(dissoc % :effects) players')]
        (-> state
            (assoc :players players'')
            (assoc :projection {:projectiles projectiles'
                                :effects     (process-effects (concat effects player-effects))
                                :limit       limit'})))
      (assoc state :current-phase :progressing))))

(defmethod weapon-firing :big-gun [state]
  (let [{:keys [angle power color p]} (get-current-player state)
        weapon (get-current-weapon state)]
    (assoc state
      :current-phase :projecting
      :projection {:projectile {:color          (:color weapon)
                                :size           5
                                :damage         100
                                :fuse           10000
                                :p              (projectile-starting-point p angle)
                                :dp             (projectile-starting-speed power angle)
                                :trail-config   (map (fn [p]
                                                       {:color (rgb-lerp color Color/DARK_GRAY p)
                                                        :width (- (* 2 bullet-size) (* p (dec (* 2 bullet-size))))})
                                                     (range 0 1.0001 1/25))
                                :trail-ps       []
                                :expire-effects (fn [p]
                                                  [(new-sparkle p 20)])
                                :hit-effects    (fn [p]
                                                  (repeatedly 5 #(new-sparkle p 20 50)))}
                   :effects    [(new-sparkle (projectile-starting-point p angle) 35)
                                (new-boom (projectile-starting-point p angle) 0 0 50)]
                   :limit      20}))) ; need a limit?

(defmethod weapon-projecting :big-gun [{:keys [projection black-holes bg-color width height players] :as state}]
  (let [{:keys [projectile effects limit]} projection
        projectile' (-> projectile
                        ((fall black-holes))
                        (trail)
                        ((hit players))
                        ((defuse state)))
        effects' (concat effects (:effects projectile'))
        done? (-> projectile' :fuse zero?)

        ;{:keys [targets p dp trail-config bullet-colors limit trail enemy-hit-index booms]} projection
        ;trail-length (count trail-config)
        ;[x y] p
        ;ddp (reduce (fn [acc black-hole] ; todo gravity-force
        ;              (map - acc (calculate-gravity (:p black-hole) (:size black-hole) p)))
        ;            [0 0]
        ;            black-holes)
        ;dp' (map + dp ddp)
        ;p' (map + p dp')
        ;enemy-hit-index' (or enemy-hit-index
        ;                     (some (fn [[idx {:keys [p]}]]
        ;                             (let [[target-x target-y] p]
        ;                               (when (hit-detected-g target-x target-y x y
        ;                                                     (/ (+ bullet-size player-size) 2))
        ;                                 (println "boom!")
        ;                                 idx)))
        ;                           targets))
        limit' (if (and done? (empty? effects)) (dec limit) 20)
        ;booms' (when enemy-hit-index'
        ;         (take 20
        ;               (conj booms
        ;                     (let [angle (rand TAU)
        ;                           r (rand player-size)]
        ;                       {:x (+ (get-in players [enemy-hit-index' :p 0]) (* r (Math/sin angle)))
        ;                        :y (+ (get-in players [enemy-hit-index' :p 1]) (* r (Math/cos angle)))
        ;                        :c (rand-nth [Color/WHITE Color/RED Color/YELLOW Color/LIGHT_GRAY Color/ORANGE])}))))
        ;trail' (take trail-length (cons (when-not enemy-hit-index' p') trail))
        ]

    ;(println ".")
    ;(println (count effects) (count (:effects projectile')) (count effects'))
    ;(Thread/sleep 5000)

    (.setColor g bg-color)
    (.fillRect g 0 0 (:width state) (+ (:height state) (:info-height state)))

    (render-players state)
    (render-black-holes state)

    (let [{:keys [p size color fuse trail-ps trail-config]} projectile']
      (doseq [[{:keys [width color]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail-ps))]
        (when (and x1 x2)
          (.setStroke g (BasicStroke. width))
          (line x1 y1 x2 y2 color)))
      (when-not (zero? fuse)
        (circle p size color)))

    ;(when-not enemy-hit-index'
    ;  (circle p (inc bullet-size) bg-color))

    ;(.setStroke g (BasicStroke. 2 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))

    ;(doseq [[{:keys [width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail))
    ;        :when x1]
    ;  (.setStroke g (BasicStroke. (+ width 2)))
    ;  (line x1 y1 x2 y2 bg-color))
    ;(doseq [[{:keys [color width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail'))
    ;        :when x1]
    ;  (.setStroke g (BasicStroke. width))
    ;  (line x1 y1 x2 y2 color))

    #_(doseq [[{:keys [color width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail))]
        (when x1

          (.setStroke g (BasicStroke. (+ width 2) BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
          (line x1 y1 x2 y2 bg-color)

          (.setStroke g (BasicStroke. width))
          (line x1 y1 x2 y2 color)))
    #_(let [[[x1 y1] [x2 y2]] (take-last 3 trail)]
        (when (and x1 x2)
          (.setStroke g (BasicStroke. 3 BasicStroke/CAP_ROUND BasicStroke/JOIN_MITER))
          (line x1 y1 x2 y2 bg-color)
          ;(.setStroke g (BasicStroke. 1))
          #_(line x2 y2 x2 y2 Color/BLACK)))

#_    (when-not enemy-hit-index'
      ;(.setStroke g (BasicStroke. 1))
      ;(line x y (+ x (* (first ddp) 100)) (+ y (* (second ddp) 100)) Color/BLUE)
      (circle p' bullet-size (first bullet-colors)))

    ;(render-players state)
    ;(render-black-holes state)

    #_(when enemy-hit-index'
        (let [[x y] (get-in players [enemy-hit-index' :p])]
          (circle x y (* player-radius 2) (rand-nth [Color/WHITE Color/YELLOW Color/RED]))))
    ;(doseq [[idx {:keys [x y c]}] (reverse (map-indexed vector booms'))]
    ;  (circle x y (+ idx player-radius) (rgba-lerp c (set-alpha c 0) (/ (count booms') 150))))

    (when (on-screen? state projectile')
      (Thread/sleep 10))

    (if (pos? limit')
      (let [players' (damage-players players [projectile'])
            player-effects (keepcat :effects players')
            players'' (mapv #(dissoc % :effects) players')]
        (-> state
            (assoc :players players'')
            (assoc :projection {:projectile projectile'
                                :effects    (process-effects (concat effects' player-effects))
                                :limit      limit'})))
      (assoc state :current-phase :progressing))))

(defmethod tick :firing [{:keys [current-player players] :as state}]
  #_(let [{:keys [angle power color p current-weapon]} (get-in state [:players current-player])
        trail-config (map (fn [p]
                            {:color (rgb-lerp color Color/DARK_GRAY p)
                             :width (- (* 2 bullet-size) (* p (dec (* 2 bullet-size))))})
                          (range 0 1.0001 1/25))
        ;bullet-colors (map (fn [p]
        ;                     (rgb-lerp Color/DARK_GRAY color p))
        ;                   (range 0 1.0001 1/30))
        weapon (nth weapons current-weapon)
        weapon-id (:id weapon)
        spread-f 0.01
        spread-fn (fn [x] (* x (+ (- 1 spread-f) (rand (* 2 spread-f)))))
        start-p (map +
                     p
                     [(* (+ player-radius gun-length) (Math/sin (degrees->radians angle)))
                      (* (+ player-radius gun-length) (Math/cos (degrees->radians angle)) -1)])
        start-dp [(* power (Math/sin (degrees->radians angle)) bullet-power-factor)
                  (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)]]
    (assoc state :projection {:targets         (but-nth current-player (map-indexed vector players))
                              :p               start-p
                              :dp              [(* power (Math/sin (degrees->radians angle)) bullet-power-factor)
                                                (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)]
                              :ps              (case (:id weapon)
                                                 :big-gun [start-p]
                                                 :shot-gun (repeat 10 start-p))
                              :dps             (case (:id weapon)
                                                 :big-gun [start-dp]
                                                 :shot-gun (repeatedly 10 #(map spread-fn start-dp)))
                              :trail-config    trail-config
                              :bullet-colors   (repeat (:color weapon)) #_(cycle (concat bullet-colors (reverse bullet-colors)))
                              :limit           10000
                              :trail           []
                              :enemy-hit-index nil
                              :booms           nil}
                 :current-phase :projecting))

  (render-info-bar (weapon-firing state)))

(defmethod tick :projecting [{:keys [projection black-holes bg-color width height players] :as state}]
  #_(let [{:keys [targets p dp ps dps trail-config bullet-colors limit trail enemy-hit-index booms]} projection
        trail-length (count trail-config)
        [x y] p
        ddp (reduce (fn [acc black-hole]
                      (map - acc (calculate-gravity (:p black-hole) (:size black-hole) p)))
                    [0 0]
                    black-holes)
        dp' (map + dp ddp)
        p' (map + p dp')
        enemy-hit-index' (or enemy-hit-index
                             (some (fn [[idx {:keys [p]}]]
                                     (let [[target-x target-y] p]
                                       (when (hit-detected-g target-x target-y x y
                                                             (/ (+ bullet-size player-size) 2))
                                         (println "boom!")
                                         idx)))
                                   targets))
        limit' (dec (cond
                      enemy-hit-index' (min limit 155)
                      (not (hit-detected-g (/ width 2) (/ height 2) x y (max width height))) 0
                      :else limit))
        booms' (when enemy-hit-index'
                 (take 20
                       (conj booms
                             (let [angle (rand TAU)
                                   r (rand player-size)]
                               {:x (+ (get-in players [enemy-hit-index' :p 0]) (* r (Math/sin angle)))
                                :y (+ (get-in players [enemy-hit-index' :p 1]) (* r (Math/cos angle)))
                                :c (rand-nth [Color/WHITE Color/RED Color/YELLOW Color/LIGHT_GRAY Color/ORANGE])}))))
        trail' (take trail-length (cons (when-not enemy-hit-index' p') trail))]

    (when-not enemy-hit-index'
      (circle p (inc bullet-size) bg-color))

    ;(.setStroke g (BasicStroke. 2 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))

    (doseq [[{:keys [width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail))
            :when x1]
      (.setStroke g (BasicStroke. (+ width 2)))
      (line x1 y1 x2 y2 bg-color))
    (doseq [[{:keys [color width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail'))
            :when x1]
      (.setStroke g (BasicStroke. width))
      (line x1 y1 x2 y2 color))

    #_(doseq [[{:keys [color width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail))]
      (when x1

        (.setStroke g (BasicStroke. (+ width 2) BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
        (line x1 y1 x2 y2 bg-color)

        (.setStroke g (BasicStroke. width))
        (line x1 y1 x2 y2 color)))
    #_(let [[[x1 y1] [x2 y2]] (take-last 3 trail)]
      (when (and x1 x2)
        (.setStroke g (BasicStroke. 3 BasicStroke/CAP_ROUND BasicStroke/JOIN_MITER))
        (line x1 y1 x2 y2 bg-color)
        ;(.setStroke g (BasicStroke. 1))
        #_(line x2 y2 x2 y2 Color/BLACK)))

    (when-not enemy-hit-index'
      ;(.setStroke g (BasicStroke. 1))
      ;(line x y (+ x (* (first ddp) 100)) (+ y (* (second ddp) 100)) Color/BLUE)
      (circle p' bullet-size (first bullet-colors)))

    (render-players state)
    (render-black-holes state)

    #_(when enemy-hit-index'
      (let [[x y] (get-in players [enemy-hit-index' :p])]
        (circle x y (* player-radius 2) (rand-nth [Color/WHITE Color/YELLOW Color/RED]))))
    (doseq [[idx {:keys [x y c]}] (reverse (map-indexed vector booms'))]
      (circle x y (+ idx player-radius) (rgba-lerp c (set-alpha c 0) (/ (count booms') 150))))

    (when (or (on-screen p' width height bullet-size)
              (on-screen (or (last trail') p') width height bullet-size)
              enemy-hit-index')
      (Thread/sleep 10))

    (if (pos? limit')
      (assoc state :projection {:targets         targets
                                :p               p'
                                :dp              dp'
                                ;:trail-colors    trail-colors
                                :trail-config    trail-config
                                :bullet-colors   (rest bullet-colors)
                                :limit           limit'
                                :trail           trail'
                                :enemy-hit-index enemy-hit-index'
                                :booms           booms'})
      (assoc state :current-phase :progressing
                   :enemy-hit-index enemy-hit-index')))

  (render-info-bar (weapon-projecting state)))

(defn process-black-holes [black-holes]
  (for [{:keys [lifecycle] :as hole} black-holes
        :when (seq lifecycle)]
    (assoc hole :size (first lifecycle)
                :lifecycle (rest lifecycle))))

(defn remove-deceased-players [{:keys [players current-player width height] :as state}]
  (reduce
    (fn [{:keys [black-holes players] :as state} deceased-index]
      (if (pos? (get-in players [deceased-index :health]))
        state
        (let [black-holes' (conj black-holes (new-black-hole (get-in players [deceased-index :p])))
              deceased (nth players deceased-index)
              reborn (assoc deceased :health 100
                                     :p (random-location width height
                                                         (concat (but-nth deceased-index players)
                                                                 black-holes'))
                                     :score (dec (:score deceased)))]
          (-> state
              (update-in [:players current-player :score] + 10)
              (assoc-in [:players deceased-index] reborn)
              (assoc :black-holes black-holes')))))
    state
    (range 0 (count players))))

(defmethod tick :progressing [{:keys [players enemy-hit-index current-player black-holes width height] :as state}]
  (while (async/poll! key-chan))
  (->
    #_(if enemy-hit-index
        (let [black-holes' (conj (process-black-holes black-holes)
                                 (new-black-hole (get-in players [enemy-hit-index :p])))]
          (-> state
              (update-in [:players current-player :score] inc)
              (assoc-in [:players enemy-hit-index :p] (random-location width height
                                                                       (concat (but-nth enemy-hit-index players)
                                                                               black-holes')))
              (assoc :black-holes black-holes')))
        state)
    state
      (update :black-holes process-black-holes)
      (remove-deceased-players)
      (update :current-player inc)
      (update :current-player mod (count players))
      (assoc :current-phase :aiming)))

(defn x []
  (let [state-0 (initial-state width height
                               ["George" "Rosa" "Rachel" "Ella"]
                               ["#FFEA00" "#00F195" "#F900FF" "#34B700"])]
    (clear state-0)
    (println (:current-phase state-0))
    (loop [state state-0]
      (let [state' (tick state)]
        (when (not= (:current-phase state) (:current-phase state'))
          (clear state')
          (println (:current-phase state')))
        (q)
        (when-not (= :exit! (:current-phase state'))
          (recur state'))))))

;(doto (initial-state width height)
;  (clear)
;  (tick))
;(q)


; TODO: ideas
; different shot types
; * parachute shot - slows down
; * guided shot, you can steer it a bit as it goes
; * braking shot, you can control the braking as it goes
; * cluster burst shot
; * spread shot
; * barrier shot, creates a barrier
; health
; * shots don't completely destroy you
; destroyable barriers
; crates
; explosions way too big

(defn play! []
  (future
    (try
      (x)
      (catch Exception e
        (.printStackTrace e)))))

(comment
  (play!))
