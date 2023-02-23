(ns core
  (:require
    [clojure.java.io :as io]
    [see :as see]
    [clojure.core.async :as async]
    [clojure.pprint :as pprint]
    [clojure.string :as string])
  (:import [java.awt.image BufferedImage]
           [java.awt Graphics2D Color BasicStroke Font RenderingHints]))


(def TAU (* Math/PI 2))

(defmacro destr
  "Create a descructurable collection of the results of calling the given accessors on the given Java object."
  [java-object & accessors]
  (let [fields (map (fn [accessor]
                      (list accessor java-object)) accessors)]
    `[~@fields]))

(defn but-nth "Returns the collection without the nth element."
  [n coll]
  (concat (take n coll)
          (drop (inc n) coll)))

(def width 2200)
(def height 1400)

(defn increase-power [power]
  (min (inc power) 80))

(defn decrease-power [power]
  (max (dec power) -0))

(defn increase-angle [angle]
  (-> angle (/ 90) inc int (* 90)))

(defn decrease-angle [angle]
  (-> angle (/ 90) dec int (* 90)))

(defonce key-chan (async/chan (async/dropping-buffer 10)))

(def !held-keys (volatile! #{}))
(defn is-held? [key-code]
  (@!held-keys key-code))

(defn key-pressed-fn [key-code]
  (vswap! !held-keys conj key-code)
  (async/>!! key-chan key-code))

(defn key-released-fn [key-code]
  (vswap! !held-keys disj key-code))

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce re-render (see/see image
                            :key-pressed-fn key-pressed-fn
                            :key-released-fn key-released-fn))
(defonce g ^Graphics2D (.getGraphics image))

(.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
(.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)


(defn rgb "Create a java.awt.Color object from the given red, green, blue and (optionally) alpha values (0-255)."
  ([red green blue]
   (rgb red green blue 255))
  ([red green blue alpha]
   (Color. (int (min 255 (max 0 red)))
           (int (min 255 (max 0 green)))
           (int (min 255 (max 0 blue)))
           (int (min 255 (max 0 alpha))))))

(defn rgba-lerp
  "Interpolate a new Color that is a proportion of the way from color-1 to color-2."
  [color-1 color-2 proportion]
  (let [r1 (.getRed color-1)
        g1 (.getGreen color-1)
        b1 (.getBlue color-1)
        a1 (.getAlpha color-1)
        r2 (.getRed color-2)
        g2 (.getGreen color-2)
        b2 (.getBlue color-2)
        a2 (.getAlpha color-2)
        ip (- 1 proportion)]
    (rgb (+ (* ip r1) (* proportion r2))
         (+ (* ip g1) (* proportion g2))
         (+ (* ip b1) (* proportion b2))
         (+ (* ip a1) (* proportion a2)))))

(defn rgb-lerp
  "Interpolate a new Color that is a proportion of the way from color-1 to color-2. Ignores alpha values."
  [color-1 color-2 proportion]
  (let [r1 (.getRed color-1)
        g1 (.getGreen color-1)
        b1 (.getBlue color-1)
        r2 (.getRed color-2)
        g2 (.getGreen color-2)
        b2 (.getBlue color-2)
        ip (- 1 proportion)]
    (rgb (+ (* ip r1) (* proportion r2))
         (+ (* ip g1) (* proportion g2))
         (+ (* ip b1) (* proportion b2)))))

(defn set-alpha "Set the alpha value of a color." [color alpha-value]
  (rgb (.getRed color) (.getGreen color) (.getBlue color) alpha-value))

(defn circle "Draw a solid circle of the given color and radius at the given location."
  ([[x y] radius color] (circle x y radius color))
  ([x y radius color]
   (.setColor g color)
   (.fillArc g
             (int (- x radius))
             (int (- y radius))
             (int (* radius 2))
             (int (* radius 2))
             0 360)))

(defn lined-circle "Draw the outline of a circle of the given radius in the given color at the given location."
  ([point radius color]
   (lined-circle point radius color 1))
  ([[x y] radius color proportion]
   (.setColor g color)
   (.drawArc g
             (int (- x radius))
             (int (- y radius))
             (int (inc (* radius 2)))
             (int (inc (* radius 2)))
             (- 180 (* 180 proportion)) (+ 180 (* 180 proportion)))))

(defn outlined-circle "Draw a solid circle and outline it in black."
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

(defn line "draw a line between two point in the given color"
  ([[x1 y1] [x2 y2] color]
   (line x1 y1 x2 y2 color))
  ([x1 y1 x2 y2 colour]
   (.setColor g colour)
   (.drawLine g (int x1) (int y1) (int x2) (int y2))))

(defn text
  [[xc yc] message color font-size]
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

(defn hit-detected? "Are two points within a certain distance of each other?"
  ([[x1 y1] [x2 y2] distance]
   (hit-detected? x1 y1 x2 y2 distance))
  ([x1 y1 x2 y2 distance]
   (let [a (- x1 x2)
         b (- y1 y2)
         c (Math/sqrt (+ (* a a) (* b b)))]
     (< c distance))))

(defn on-screen "Is the given point within a rectangle smaller than the size by the margin."
  [[x y] width height margin]
  (and (< margin x (- width margin))
       (< margin y (- height margin))))

(defn angle-and-distance "return the angle and distance between two points"
  [[x1 y1] [x2 y2]]
  (let [x-diff (- x1 x2)
        y-diff (- y1 y2)
        angle (Math/atan2 y-diff x-diff)
        distance (Math/sqrt (+ (* x-diff x-diff)
                               (* y-diff y-diff)))]
    [angle distance]))

(defn calculate-gravity
  "Returns a vector representing the attractive force between two bodies."
  [position-of-mass size-of-mass position]
  (let [[angle distance] (angle-and-distance position position-of-mass)
        distance' (/ distance 10)
        force (/ size-of-mass (* distance' distance'))
        force-x (* force (Math/cos angle))
        force-y (* force (Math/sin angle))]
    [force-x force-y]))

;;;;;;;;;;;;;;;;;;;;;;;;

(def gun-length 20)
(def player-radius 20)

(defn random-location
  "Returns a new random location in the game-space that has enough space to fit a player in it."
  [width height anti-targets]
  (let [margin (+ player-radius gun-length 10)
        x (+ margin (rand-int (- width margin margin)))
        y (+ margin (rand-int (- height margin margin)))]
    (if (not-any? #(hit-detected? x y
                                  (first (:p %)) (second (:p %))
                                  (+ (abs (:size %)) margin))
                  anti-targets)
      [x y]
      (recur width height anti-targets))))

(defn new-singularity "Creates a new black or white hole."
  [point]
  (let [[lifecycle color] (if (< 0.75 (rand))
                            [(range -25 -2 0.4) Color/WHITE]
                            [(range 25 2 -0.3) Color/BLACK])]
    {:p         point
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
     :bg-color      Color/GRAY
     :current-phase :initializing}))

(defmethod tick :initializing [state]
  (assoc state :players [{:label "Start game" :kind :start}
                           {:label "Add a new player" :kind :add}
                           {:label "You" :kind :human}
                           {:label "Me" :kind :ai}]
               :current-player 0
               :current-phase :menu))

(def colors
  (->> (for [h (range 0.0 2.9 0.4)]
         (Color/getHSBColor h 1.0 1.0))
       (map (fn [^Color c]
              (let [light (/ (+ (.getRed c) (.getGreen c) (.getBlue c))
                             (+ 255 255 255))]
                (rgb-lerp c Color/WHITE (* (- 1 light) 3/5)))))
       (reverse)))

(def weapons
  [{:id    :pass
    :label "Pass"
    :cost  -30
    :color (Color. (Integer/decode "#FFFFFF"))}
   {:id    :invest
    :label "Invest"
    :cost  10
    :color (Color. (Integer/decode "#77C1FF"))}
   {:id    :heal
    :label "Repair"
    :cost  10
    :color (Color. (Integer/decode "#26D900"))}
   {:id    :big-gun
    :label "The Big One"
    :cost  100
    :color (Color. (Integer/decode "#FF0000"))}
   {:id    :cheap-gun
    :label "The Cheap One"
    :cost  20
    :color (Color. (Integer/decode "#FDA46D"))}
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

(defn get-current-player [{:keys [players current-player]}]
  (get players current-player))

(defn get-current-weapon [state]
  (nth weapons (:current-weapon (get-current-player state))))

(defn await-entry [{:keys [current-player players width height] :as state}]
  (if-let [key-code (async/<!! key-chan)]
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
             :add (if (< (-> state :players count) 10)
                    (update state :players conj {:label "Also Me" :kind :ai})
                    state)
             :start (-> state
                        (update :players initialize-players width height)
                        (assoc :black-holes [(new-singularity [(* width 1/2) (* height 1/2)])]
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
          state)))
    (assoc state :current-phase :exit!)))

(defmethod tick :menu [{:keys [players current-player width height info-height] :as state}]
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
  (re-render)
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

(defn render-singularities [{:keys [black-holes]}]
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
          (str
            (if (pos? (:cost weapon)) "-" "+")
            "$" (Math/abs (:cost weapon)) " : " (:label weapon))
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
            message (str label ": $" (int cash) " / " score "pts.")
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
      (let [target (rand-nth (but-nth current-player players))
            weapon-idx (rand-nth (keep-indexed (fn [idx weapon]
                                                 (when (string/ends-with? (str (:id weapon)) "gun") idx))
                                               weapons))
            cash (:cash (get-current-player state))
            cost (:cost (nth weapons weapon-idx))
            [weapon-idx' cash'] (if (< cash cost)
                                  [0 (- cash (:cost (nth weapons 0)))]
                                  [weapon-idx (- cash cost)])]
        (Thread/sleep 1000)
        (-> state
            (update-in [:players current-player] merge
                       {:current-weapon weapon-idx'
                        :cash           cash'
                        :power          (+ 5 (rand-int 75))
                        :angle          (-> (first (angle-and-distance p (:p target)))
                                            (/ TAU) (* 360) (- 90) (+ (rand-int 11) -5))})
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
  (render-singularities state)
  (render-info-bar state)
  (re-render)
  (await-command state))

(defn projectile-starting-point [p angle]
  (map +
       p
       [(* (+ player-radius gun-length) (Math/sin (degrees->radians angle)))
        (* (+ player-radius gun-length) (Math/cos (degrees->radians angle)) -1)]))

(def bullet-power-factor 0.2)

(defn projectile-starting-speed [power angle]
  [(* power (Math/sin (degrees->radians angle)) bullet-power-factor)
   (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)])

(defn gravity-force [point other-masses]
  (reduce (fn [acc mass]
            (map - acc (calculate-gravity (:p mass) (:size mass) point)))
          [0 0]
          other-masses))

(defmulti process-effect :effect-type)

(defn new-sparkle
  "A sudden white circle that rapidly shrinks."
  ([p size]
   {:effect-type :sparkle :p p :size size})
  ([p offset size]
   (let [angle (rand TAU)
         r (rand offset)
         v [(* r (Math/sin angle)) (* r (Math/cos angle))]]
     (new-sparkle (map + p v) size))))

(defmethod process-effect :sparkle [{:keys [p size] :as sparkle}]
  (circle p size Color/WHITE)
  (when (pos? size)
    (update sparkle :size - 5)))

(defn new-boom
  "A delayed random-explosion-colored circle that fades as it grows."
  [p delay offset size]
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

(defmethod process-effect :boom [{:keys [p lifecycle] :as sparkle}]
  (when (seq lifecycle)
    (let [{:keys [size color]} (first lifecycle)]
      (circle p size color))
    (update sparkle :lifecycle rest)))

(defn new-shockwave
  "A rapidly growing and fading circle with a darker outline."
  [p size]
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

(defn process-effects [effects]
  (doall (keep process-effect effects)))

(defmulti weapon-firing (comp :id get-current-weapon))
(defmulti weapon-projecting (comp :id get-current-weapon))

(defn realize-returns [{:keys [cash investments] :as player}]
  (-> player
      (assoc :cash (apply + cash (map first investments)))
      (update :investments (partial keep next))))

(defn invest [player]
  (-> player
      (update :investments conj (repeat 20 1))))

(defmethod weapon-firing :invest [{:keys [current-player] :as state}]
  (-> state
      (update-in [:players current-player] invest)
      (assoc :current-phase :aiming)))

(defmethod weapon-firing :pass [{:keys [players current-player] :as state}]
  (-> state
      (update-in [:players current-player] realize-returns)
      (assoc :current-phase :progressing
             :current-player (mod (inc current-player) (count players)))))

(defmethod weapon-firing :heal [{:keys [current-player] :as state}]
  (-> state
      (update-in [:players current-player :health] (fn [health] (min 100 (+ health 15))))
      (assoc :current-phase :aiming)))

(defmethod weapon-firing :shot-gun [{:keys [bg-color] :as state}]
  (let [{:keys [angle power color p]} (get-current-player state)
        weapon (get-current-weapon state)
        spread-f 0.07
        spread-fn (fn [x]
                    (* x (+ (- 1 spread-f) (rand (* 2 spread-f)))))]
    (assoc state
      :current-phase :projecting
      :projection {:projectiles (repeatedly 10 (fn []
                                                 {:color          (:color weapon)
                                                  :size           3
                                                  :damage         10
                                                  :fuse           (+ 500 (rand-int 10))
                                                  :p              (projectile-starting-point p angle)
                                                  :dp             (map spread-fn
                                                                       (projectile-starting-speed power angle))
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

(defn clear-one-offs [projectile]
  (dissoc projectile :effects :player-damages))

(defn fall
  "Returns a function that moves a projectile with its current momentum and accelerates it towards the given masses.
  Also burns the fuse on the projectile."
  [masses]
  (fn [{:keys [p dp fuse expire-effects] :as projectile}]
    (if (zero? fuse)
      projectile
      (let [ddp (gravity-force p masses)
            dp' (map + dp ddp)
            p' (map + p dp')
            fuse' (dec fuse)]
        (if (pos? fuse')
          (assoc projectile :p p' :dp dp' :fuse fuse')
          (assoc projectile :fuse 0
                            :effects (expire-effects p')))))))

(defn thrust
  "Allows manual steering of guided projectiles with the left and right arrow keys."
  [{:keys [dp] :as projectile}]
  (let [effectiveness 1/100]
    (cond
      (is-held? 39) (let [[current-angle distance] (angle-and-distance dp [0 0])
                          new-angle (+ current-angle (* TAU effectiveness))]
                      (assoc projectile :dp [(* distance (Math/cos new-angle))
                                             (* distance (Math/sin new-angle))]))
      (is-held? 37) (let [[current-angle distance] (angle-and-distance dp [0 0])
                          new-angle (- current-angle (* TAU effectiveness))]
                      (assoc projectile :dp [(* distance (Math/cos new-angle))
                                             (* distance (Math/sin new-angle))]))
      :else projectile)))

(defn detonate
  "Allows for early detonation when the Enter key is pressed."
  [players]
  (fn [{:keys [p hit-effects damage fuse] :as projectile}]
    (if (and (= 10 (async/poll! key-chan))
             (pos? fuse))
      (assoc projectile :fuse 0
                        :effects (hit-effects p)
                        :player-damages (keep-indexed (fn [idx {:keys [size] :as player}]
                                                        (let [d (- damage (- (distance p (:p player)) size))]
                                                          (when (pos? d)
                                                            [idx d])))
                                                      players))
      projectile)))

(defn trail "Step function for projectile trails."
  [{:keys [p trail-ps trail-config fuse] :as projectile}]
  (assoc projectile :trail-ps (take (count trail-config) (conj trail-ps (if (zero? fuse) nil p)))))

(defn hit
  "Returns a function to process projectiles hitting players."
  [players]
  (fn [{:keys [hit-player? p damage hit-effects] :as projectile}]
    (if hit-player?
      projectile
      (if-let [hit-player (first
                            (keep-indexed (fn [idx player]
                                            (when (hit-detected? (:p player) (:p projectile)
                                                                 (+ (:size player) (:size projectile)))
                                              idx))
                                          players))]
        (assoc projectile :player-damages [[hit-player damage]]
                          :hit-player? true
                          :fuse 0
                          :effects (hit-effects p))
        projectile))))

(defn defuse
  "Defuse a projectile if it leaves the game area and is unlikely to return."
  [{:keys [width height]}]
  (fn [{:keys [p] :as projectile}]
    (if (hit-detected? p [(/ width 2) (/ height 2)] (max width height))
      projectile
      (assoc projectile :fuse 0))))

(defn on-screen?
  "Is the projectile and its trail on screen?"
  [{:keys [width height]} {:keys [p size trail-ps]}]
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
              (update players idx assoc
                      :health health'
                      :effects effects)))
          players
          (mapcat :player-damages projectiles)))

(defmethod weapon-projecting :shot-gun [{:keys [projection black-holes bg-color players] :as state}]
  (let [{:keys [projectiles limit effects]} projection
        projectiles' (map (comp (defuse state)
                                (hit players)
                                trail
                                (fall black-holes)
                                clear-one-offs)
                          projectiles)
        effects (concat effects (mapcat :effects projectiles'))
        all-done? (every? #(-> % :fuse zero?) projectiles')
        limit' (if (and all-done? (empty? effects)) (dec limit) 20)]

    (.setColor g bg-color)
    (.fillRect g 0 0 (:width state) (+ (:height state) (:info-height state)))

    (render-players state)
    (render-singularities state)

    (doseq [{:keys [p size color fuse trail-ps trail-config]} projectiles']
      (doseq [[{:keys [width color]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail-ps))]
        (when (and x1 x2)
          (.setStroke g (BasicStroke. width))
          (line x1 y1 x2 y2 color)))
      (when-not (zero? fuse)
        (circle p size color)))

    (when (some (partial on-screen? state) projectiles')
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
                        (clear-one-offs)
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
    (render-singularities state)

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
                        (clear-one-offs)
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
    (render-singularities state)

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
                        (clear-one-offs)
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
    (render-singularities state)

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

(defn process-singularities [black-holes]
  (for [{:keys [lifecycle] :as hole} black-holes
        :when (seq lifecycle)]
    (assoc hole :size (first lifecycle)
                :lifecycle (rest lifecycle))))

(defn remove-deceased-players [{:keys [players current-player width height] :as state}]
  (reduce
    (fn [{:keys [black-holes players] :as state} deceased-index]
      (if (pos? (get-in players [deceased-index :health]))
        state
        (let [black-holes' (conj black-holes (new-singularity (get-in players [deceased-index :p])))
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
      (update :black-holes process-singularities)
      (remove-deceased-players)
      (assoc :current-phase :aiming)))

(defn tick-safely [state]
  (try
    (tick state)
    (catch Exception e
      (println "tick FAILED:" (.getSimpleName (class e)) (ex-message e))
      (spit (io/file "state-dump.edn")
            (with-out-str
              (pprint/pprint state)))
      (throw e))))

(defn clear-screen [{:keys [bg-color width height info-height]}] ; todo: use this elsewhere!
  (.setColor g bg-color)
  (.fillRect g 0 0 width (+ height info-height)))

(defn game-loop []
  (loop [state (initial-state width height)]
    ; TODO: catch exceptions and log state!!!
    (let [state' (tick-safely state)]
      (when (not= (:current-phase state) (:current-phase state'))
        (println "->" (name (:current-phase state')))
        (clear-screen state'))
      (re-render)
      (when-not (= :exit! (:current-phase state'))
        (recur state')))))


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
; damage effect, print the amount of damage done above the player being damaged

; highlight whose turn it is more!!!
; button to pass
; better fit info on the info bar when there are many players

(defn play!
  "Start the game loop in a future - this prevents the REPL being blocked and allows hot-loading changes into the
  running game."
  []
  (future
    (try
      (game-loop)
      (catch Exception e
        (.printStackTrace e)))))

(comment
  (play!))
