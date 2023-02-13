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

(def width 2200)
(def height 1400)
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

(defn lined-circle
  ([[x y] radius color] (lined-circle x y radius color))
  ([x y radius color]
   (.setColor g color)
   (.drawArc g
             (int (- x radius))
             (int (- y radius))
             (int (inc (* radius 2)))
             (int (inc (* radius 2)))
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
             (int (- x radius))
             (int (- y radius))
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

(defn radians->degrees [radians]
  (-> radians (* 360) (/ TAU)))

(defn distance [[x1 y1] [x2 y2]]
  (let [a (- x1 x2)
        b (- y1 y2)]
    (Math/sqrt (+ (* a a) (* b b)))))

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
    [angle distance]))

(defn calculate-gravity [mp size p]
  (let [[angle distance] (angle-and-distance p mp)
        distance' (/ distance 10)
        force (/ size (* distance' distance'))
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

(defn initial-state [width height]
  (let [info-height 50
        game-width width
        game-height (- height info-height)]
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
  [{:id    :pass
    :label "Pass"
    :cost  0
    :color (Color. (Integer/decode "#FFFFFF"))}
   {:id    :big-gun
    :label "The Big One"
    :cost  100
    :color (Color. (Integer/decode "#FF0000"))}
   {:id    :cheap-gun
    :label "The Cheap One"
    :cost  20
    :color (Color. (Integer/decode "#00FF0C"))}
   {:id    :shot-gun
    :label "Shotgun"
    :cost  60
    :color (Color. (Integer/decode "#1100FF"))}
   {:id    :guided-shot
    :label "Guided Missile"
    :cost  80
    :color (Color. (Integer/decode "#FFEA00"))}])

(defn health->color [health]
  (if (< 50 health)
    (rgb-lerp Color/YELLOW Color/GREEN (/ (- health 50) 50))
    (rgb-lerp Color/RED Color/YELLOW (/ health 50))))

(defn initialize-players [players game-width game-height]
  (->> players
       (filter (comp #{:human :ai} :kind))
       (mapv #(assoc % :size player-radius
                       :score 0
                       :health 100
                       :cash 30
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
  (get players current-player))

(defn get-current-weapon [state]
  (nth weapons (:current-weapon (get-current-player state))))

(defn await-entry [{:keys [current-player players width height] :as state}]
  (if-let [key-code (async/<!! key-chan)]
    (do
      (case key-code
        38 (assoc state :current-player (mod (dec current-player) (count players)))
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

          (.setStroke g (BasicStroke. 5))
          (outlined-circle p player-radius color)
          (.setStroke g (BasicStroke. 2))
          (lined-circle p player-radius (health->color health))

          (when aiming?
            (text (map + p [0 1.5]) (str power) Color/BLACK 23)))))))

(defn render-black-holes [{:keys [black-holes]}]
  (doseq [{:keys [p size]} black-holes]
    (let [[x y] p]
      (circle x y (abs size) (if (pos? size)
                               Color/BLACK
                               Color/WHITE)))))

(defn render-info-bar [{:keys [current-player players width height info-height] :as state}]
  (.setFont g (Font. Font/SANS_SERIF Font/BOLD 24))
  (let [weapon-section 500
        player-width (/ (- width weapon-section) (count players))
        weapon (get-current-weapon state)
        player (get-current-player state)]
    (.setColor g Color/DARK_GRAY)
    (.fillRect g 0 height width info-height)
    (.setColor g Color/BLACK)
    (.fillRect g (- width weapon-section) height weapon-section info-height)
    (.setColor g Color/WHITE)
    (text [(- width (* weapon-section 1/2))
           (int (+ height (* info-height 1/2)))]
          (str "$" (:cost weapon) " : " (:label weapon))
          (if (< (:cash player) (:cost weapon))
            Color/RED
            Color/WHITE)
          40)

    (.setFont g (Font. Font/SANS_SERIF Font/BOLD 30))
    (doseq [[idx {:keys [score color label cash health]}] (reverse (map-indexed vector players))]
      (let [corner-diameter 40
            h-inset 15
            xc (* player-width (+ idx 1/2))
            yc (+ height (/ info-height 2))
            message (str label ": $" (int cash) " / " score "pts.") ; todo: render health properly! maybe on the player as well?
            raw-label-width (-> g .getFontMetrics (.getStringBounds message g) .getWidth)
            label-width (+ raw-label-width h-inset h-inset)
            label-height info-height
            left (- xc (/ label-width 2))
            top (- yc (/ label-height 2))]
        (.setColor g color)
        (.fillRoundRect g (- left player-width) top (+ label-width player-width) label-height corner-diameter corner-diameter)
        (.setStroke g (BasicStroke. 1))
        (.setColor g Color/BLACK)
        (.drawRoundRect g (- left player-width) top (+ label-width player-width) label-height corner-diameter corner-diameter)
        (.setColor g Color/DARK_GRAY)
        (.fillRect g (- xc (/ raw-label-width 2)) (+ yc 5) raw-label-width 10)
        (.setColor g (health->color health))
        (.fillRect g (- xc (/ raw-label-width 2)) (+ yc 5) (* raw-label-width (/ health 100)) 10)
        (.setColor g Color/BLACK)
        (.drawRect g (- xc (/ raw-label-width 2)) (+ yc 5) raw-label-width 10)
        (text [xc (- yc 5)] message (if (= current-player idx) Color/WHITE Color/BLACK) 30))))
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
          44 (update-in state [:players current-player :current-weapon] (fn [x] (mod (inc x) (count weapons))))
          46 (update-in state [:players current-player :current-weapon] (fn [x] (mod (dec x) (count weapons))))
          10 (let [cash (:cash (get-current-player state))
                   cost (:cost (get-current-weapon state))
                   cash' (- cash cost)]
               (if (neg? cash')
                 state
                 (-> state
                     (assoc :current-phase :firing)
                     (assoc-in [:players current-player :cash] cash'))))
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

(defn new-shockwave [p size]
  (let [color (Color. (Integer/decode "#42efff"))]
    {:effect-type :shockwave
     :p           p
     :lifecycle   (map (fn [s]
                         {:size  s
                          :line (rgba-lerp color (set-alpha color 0) (Math/pow (/ s size) 4))
                          :fill (rgba-lerp color (set-alpha color 0) (/ s size))})
                       (range 0 size 7))}))

(defmethod process-effect :shockwave [{:keys [p lifecycle] :as shockwave}]
  (when (seq lifecycle)
    (let [{:keys [size line fill]} (first lifecycle)]
      (circle p size fill)
      (lined-circle p size line))
    (update shockwave :lifecycle rest)))

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

(defmethod weapon-firing :pass [{:keys [players current-player] :as state}]
  (-> state
      (update-in [:players current-player :cash] + 30)
      (assoc :current-phase :progressing
             :current-player (mod (inc current-player) (count players)))))

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

(defn thrust [{:keys [dp] :as projectile}]
  (let [[current-angle distance] (angle-and-distance dp [0 0])
        current-key (async/poll! key-chan)]
    (case current-key
      39 (let [new-angle (+ current-angle (* TAU 1/40))]
           (assoc projectile :dp [(* distance (Math/cos new-angle))
                                  (* distance (Math/sin new-angle))]))
      37 (let [new-angle (- current-angle (* TAU 1/40))]
           (assoc projectile :dp [(* distance (Math/cos new-angle))
                                  (* distance (Math/sin new-angle))]))
      projectile)))

(defn detonate [players]
  (fn [{:keys [p hit-effects damage] :as projectile}]
    (if (= 10 (async/poll! key-chan))
      (assoc projectile :fuse 0
                        :effects (hit-effects p)
                        :player-damages (keep-indexed (fn [idx {:keys [size] :as player}]
                                                        (let [d (- damage (- (distance p (:p player)) size))]
                                                          (when (pos? d)
                                                            [idx d])))
                                                      players))
      projectile)))

(defn trail [{:keys [p trail-ps trail-config fuse] :as projectile}]
  (assoc projectile :trail-ps (take (count trail-config) (conj trail-ps (if (zero? fuse) nil p)))))

(defn hit [players]
  (fn [{:keys [hit-player? p damage hit-effects] :as projectile}]
    (if hit-player?
      (dissoc projectile :player-damages)
      (if-let [hit-player (first
                            (keep-indexed (fn [idx player]
                                            (when (hit-detected-g (:p player) (:p projectile)
                                                                  (+ (:size player) (:size projectile)))
                                              idx))
                                          players))]
        (assoc projectile :player-damages [[hit-player damage]]
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
  (and (< (- size) (first p) (+ width size))
       (< (- size) (second p) (+ height size))
       (if-let [last-trail-p (last trail-ps)]
         (and (< (- size) (first last-trail-p) (+ width size))
              (< (- size) (second last-trail-p) (+ height size)))
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
          (mapcat :player-damages projectiles)))

(defmethod weapon-projecting :shot-gun [{:keys [projection black-holes bg-color players] :as state}]
  (let [{:keys [projectiles limit effects]} projection
        projectiles' (map (comp (defuse state)
                                (hit players)
                                trail
                                (fall black-holes))
                          projectiles)
        effects (concat effects (mapcat :effects projectiles'))
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
            player-effects (mapcat :effects players')
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
                                                        :width (- 8 (* p 7))})
                                                     (range 0 1.0001 1/25))
                                :trail-ps       []
                                :expire-effects (fn [p]
                                                  [(new-sparkle p 20)])
                                :hit-effects    (fn [p]
                                                  (cons
                                                    (new-shockwave p 120)
                                                    (repeatedly 5 #(new-sparkle p 20 50))))}
                   :effects    [(new-sparkle (projectile-starting-point p angle) 35)
                                (new-boom (projectile-starting-point p angle) 0 0 50)]})))

(defmethod weapon-projecting :big-gun [{:keys [projection black-holes bg-color players] :as state}]
  (let [{:keys [projectile effects limit]} projection
        projectile' (-> projectile
                        ((fall black-holes))
                        (trail)
                        ((hit players))
                        ((defuse state))
                        ((detonate players)))
        effects' (concat effects (:effects projectile'))
        done? (-> projectile' :fuse zero?)
        limit' (if (and done? (empty? effects)) (dec (or limit 20)) 20)]

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

    (when (on-screen? state projectile')
      (Thread/sleep 10))

    (if (pos? limit')
      (let [players' (damage-players players [projectile'])
            player-effects (mapcat :effects players')
            players'' (mapv #(dissoc % :effects) players')]
        (-> state
            (assoc :players players'')
            (assoc :projection {:projectile projectile'
                                :effects    (process-effects (concat effects' player-effects))
                                :limit      limit'})))
      (assoc state :current-phase :progressing))))

(defmethod weapon-firing :cheap-gun [state]
  (let [{:keys [angle power color p]} (get-current-player state)
        weapon (get-current-weapon state)]
    (assoc state
      :current-phase :projecting
      :projection {:projectile {:color          (:color weapon)
                                :size           3
                                :damage         40
                                :fuse           1000
                                :p              (projectile-starting-point p angle)
                                :dp             (projectile-starting-speed power angle)
                                :trail-config   (map (fn [p]
                                                       {:color (rgb-lerp color Color/DARK_GRAY p)
                                                        :width (- 4 (* p 3))})
                                                     (range 0 1.0001 1/15))
                                :trail-ps       []
                                :expire-effects (fn [p]
                                                  [(new-sparkle p 15)])
                                :hit-effects    (fn [p]
                                                  (repeatedly 5 #(new-sparkle p 20 25)))}
                   :effects    [(new-sparkle (projectile-starting-point p angle) 25)]})))

(defmethod weapon-projecting :cheap-gun [{:keys [projection black-holes bg-color players] :as state}]
  (let [{:keys [projectile effects limit]} projection
        projectile' (-> projectile
                        ((fall black-holes))
                        (trail)
                        ((hit players))
                        ((defuse state)))
        effects' (concat effects (:effects projectile'))
        done? (-> projectile' :fuse zero?)
        limit' (if (and done? (empty? effects)) (dec (or limit 20)) 20)]

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

    (when (on-screen? state projectile')
      (Thread/sleep 10))

    (if (pos? limit')
      (let [players' (damage-players players [projectile'])
            player-effects (mapcat :effects players')
            players'' (mapv #(dissoc % :effects) players')]
        (-> state
            (assoc :players players'')
            (assoc :projection {:projectile projectile'
                                :effects    (process-effects (concat effects' player-effects))
                                :limit      limit'})))
      (assoc state :current-phase :progressing))))

(defmethod weapon-firing :guided-shot [state]
  (let [{:keys [angle power color p]} (get-current-player state)
        weapon (get-current-weapon state)]
    (assoc state
      :current-phase :projecting
      :projection {:projectile {:color          (:color weapon)
                                :size           7
                                :damage         60
                                :fuse           10000
                                :p              (projectile-starting-point p angle)
                                :dp             (projectile-starting-speed power angle)
                                :trail-config   (map (fn [p]
                                                       {:color (rgb-lerp color Color/DARK_GRAY p)
                                                        :width 1})
                                                     (range 0 1.0001 1/20))
                                :trail-ps       []
                                :expire-effects (fn [p]
                                                  [(new-sparkle p 30)])
                                :hit-effects    (fn [p]
                                                  (repeatedly 5 #(new-sparkle p 20 40)))}
                   :effects    [(new-sparkle (projectile-starting-point p angle) 15)]
                   :limit      20})))

(defmethod weapon-projecting :guided-shot [{:keys [projection black-holes bg-color players] :as state}]
  (let [{:keys [projectile effects limit]} projection
        projectile' (-> projectile
                        ((fall black-holes))
                        (thrust)
                        (trail)
                        ((hit players))
                        ((defuse state)))
        effects' (concat effects (:effects projectile'))
        done? (-> projectile' :fuse zero?)
        limit' (if (and done? (empty? effects)) (dec limit) 20)]

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

    (when (on-screen? state projectile')
      (Thread/sleep 10))

    (if (pos? limit')
      (let [players' (damage-players players [projectile'])
            player-effects (mapcat :effects players')
            players'' (mapv #(dissoc % :effects) players')]
        (-> state
            (assoc :players players'')
            (assoc :projection {:projectile projectile'
                                :effects    (process-effects (concat effects' player-effects))
                                :limit      limit'})))
      (assoc state :current-phase :progressing))))

(defmethod tick :firing [state]
  (render-info-bar (weapon-firing state)))

(defmethod tick :projecting [state]
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

(defmethod tick :progressing [state]
  (while (async/poll! key-chan))
  (-> state
      (update :black-holes process-black-holes)
      (remove-deceased-players)
      ;(update :current-player inc)
      ;(update :current-player mod (count (:players state)))
      (assoc :current-phase :aiming)))

(defn x []
  (let [state-0 (initial-state width height)]
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


; TODO: ideas
; different shot types
; * parachute shot - slows down
; * guided shot, you can steer it a bit as it goes
; * timed shot, you set the fuse
; * proximity shot, blows up when it starts getting further away from an enemy
; * braking shot, you can control the braking as it goes
; * cluster burst shot
; * spread shot - DONE
; * barrier shot, creates a barrier
; health
; * shots don't completely destroy you - DONE
; destroyable barriers
; crates
; explosions way too big - ? maybe done

(defn play! []
  (future
    (try
      (x)
      (catch Exception e
        (.printStackTrace e)))))

(comment
  (play!))
