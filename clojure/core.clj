(ns core
  (:require [see :as see]
            [clojure.core.async :as async])
  (:import [java.awt.image BufferedImage]
           [java.awt Graphics2D Color BasicStroke Font RenderingHints]))


(println "Hello")

(def TAU (* Math/PI 2))

(def width 2200)
(def height 1400)
(def player-size 40)
(def bullet-size 10)

(def current-player-state (atom nil))

(defn increase-power [power]
  (min (inc power) 80))

(defn decrease-power [power]
  (max (dec power) -0))

(defn increase-angle [angle]
  (-> angle (/ 90) inc int (* 90)))

(defn decrease-angle [angle]
  (-> angle (/ 90) dec int (* 90)))

(defn key-handler-fn [key-code]
  (when-let [player-state @current-player-state]
    (case key-code
      37 (swap! player-state update :angle dec)
      38 (swap! player-state update :power increase-power)
      33 (swap! player-state update :angle increase-angle)
      34 (swap! player-state update :angle decrease-angle)
      39 (swap! player-state update :angle inc)
      40 (swap! player-state update :power decrease-power)
      10 (swap! player-state assoc :ready? true)
      27 (swap! player-state assoc :quit? true)
      (println key-code))
    #_(println @player-state)))

(defonce key-chan (async/chan (async/dropping-buffer 10)))

(defn key-handler-fn [key-code]
  (async/>!! key-chan key-code))

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce q (see/see image :key-handler-fn key-handler-fn))
(defonce g ^Graphics2D (.getGraphics image))

(.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
(.setRenderingHint g RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)


;(def p q)
;(def q (constantly nil))


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

(defn line [x1 y1 x2 y2 colour]
  (.setColor g colour)
  (.drawLine g (int x1) (int y1) (int x2) (int y2)))

(defn degrees->radians [degrees]
  (-> degrees (/ 360) (* TAU)))

(defn draw-player [player]
  (let [gun-length 40
        power (:power @(:egroeg player))]

    (circle @(:x player)
            @(:y player)
            (inc (inc (+ player-size gun-length)))
            (:bg-colour player))

    (.setStroke g (BasicStroke. 1))
    (outlined-circle @(:x player)
                     @(:y player)
                     (+ player-size (* 1/4 gun-length))
                     (rgba-lerp Color/BLACK (:bg-colour player) 0.75))

    (.setStroke g (BasicStroke. 10 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
    (let [angle (:angle @(:egroeg player))]
      (line @(:x player)
            @(:y player)
            (+ @(:x player) (* gun-length (Math/sin (degrees->radians angle))))
            (+ @(:y player) (* gun-length (Math/cos (degrees->radians angle)) -1))
            Color/BLACK))

    (.setStroke g (BasicStroke. 3))
    (outlined-circle @(:x player)
                     @(:y player)
                     player-size
                     (:colour player))

    (.setColor g Color/BLACK)
    (.setFont g (Font. Font/SANS_SERIF Font/BOLD (if (= power 100) 18 24)))
    (let [bounds (-> g .getFontMetrics (.getStringBounds (str power) g))]
      (.drawString g (str power)
                   (int (- @(:x player) (.getCenterX bounds)))
                   (int (- @(:y player) (.getCenterY bounds)))))))

(defn hit-detected-g [x1 y1 x2 y2 distance]
  (let [a (- x1 x2)
        b (- y1 y2)
        c (Math/sqrt (+ (* a a) (* b b)))]
    (< c distance)))

(defn on-screen [[x y] width height margin]
  (and (< margin x (- width margin))
       (< margin y (- height margin))))

(defn draw-score [player x y]
  (.setFont g (Font. Font/SANS_SERIF Font/BOLD 24))
  (let [score (str @(:score player))
        colour (:colour player)
        bounds (-> g .getFontMetrics (.getStringBounds score g))
        circle-radius 25]
    (.setColor g colour)
    (.fillArc g
              (- x circle-radius)
              (- y circle-radius)
              (* circle-radius 2)
              (* circle-radius 2)
              0 360)
    (.setColor g Color/BLACK)
    (.drawString g (str score)
                 (int (- x (.getCenterX bounds)))
                 (int (- y (.getCenterY bounds))))))

(defn draw-scores [player other-player]
  (let [sorted-players (sort-by :name [player other-player])]
    (draw-score (first sorted-players) (- (/ width 2) 50) 50)
    (draw-score (second sorted-players) (+ (/ width 2) 50) 50)))

(defn draw-black-hole [x y]
  (circle x y 50 Color/BLACK))

(defn player-choosing-angle [player other-player]

  ;(println "player-choosing-angle")
  (let [state (:egroeg player)]
    (swap! state assoc :ready? false)
    (reset! current-player-state state)

    ;(println "looping...")
    (loop []
      (when-not (or (:ready? @state)
                    (:quit? @state))
        ; draw barrel at angle
        ;(println "------>" (:ready? @state))
        (draw-player player)
        (draw-black-hole (/ width 2) (/ height 2))
        (draw-black-hole (* width 4/5) (* height 1/5))
        (draw-scores player other-player)
        (q)
        ;(println @@current-player-state)
        ;(println @state)
        ;(println player)

        (Thread/sleep 100)
        (recur))
      )
    ;(println "loop ended")
    ))

(def gravity 0.1)

(defn angle-and-distance [x1 y1 x2 y2]
  (let [x-diff (- x1 x2)
        y-diff (- y1 y2)
        angle (Math/atan2 y-diff x-diff)
        distance (Math/sqrt (+ (* x-diff x-diff)
                               (* y-diff y-diff)))]
    [angle (/ distance 10)]))

(defn calculate-gravity [mx my size x y]
  (let [[angle distance] (angle-and-distance x y mx my)
        force (/ size (* distance distance))
        force-x (* force (Math/cos angle))
        force-y (* force (Math/sin angle))]
    [force-x force-y]))

(defn turn [player other-player]
  (player-choosing-angle player other-player)
  (let [angle (:angle @(:egroeg player))
        power (:power @(:egroeg player))
        bullet-power-factor 0.2
        trail-colours (map (fn [p]
                             (rgb (+ p (.getRed (:bg-colour player)))
                                  (+ p (.getGreen (:bg-colour player)))
                                  (+ p (.getBlue (:bg-colour player)))))
                           (range 250 -1 -10))
        trail-length (count trail-colours)]
    (loop [x @(:x player)
           y @(:y player)
           dx (* power (Math/sin (degrees->radians angle)) bullet-power-factor)
           dy (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)
           ddx 0
           ddy gravity
           limit 1000
           trail []
           boom? false
           booms nil]
      (let [

            [gravity-x gravity-y] (map - (map +
                                              (calculate-gravity (/ width 2) (/ height 2) 100 x y)
                                              (calculate-gravity (* width 4/5) (* height 1/5) 100 x y)))

            ;_ (println (/ width 2) (/ height 2) x y gravity-x gravity-y)

            ddx' gravity-x
            ddy' (+ #_gravity gravity-y)
            dx' (+ dx ddx')
            dy' (+ dy ddy')
            x' (+ x dx')
            y' (+ y dy')


            limit' (dec (if (and (< y (+ bullet-size height))
                                 (< 0 x width))
                          limit
                          (min limit 200)))
            boom?' (or boom?
                       (when (hit-detected-g @(:x other-player) @(:y other-player)
                                             x y
                                             (/ (+ bullet-size player-size) 2))
                         (println "BOOM!!!")
                         (swap! (:egroeg other-player) assoc :dead? true)
                         true))
            booms' (when boom?
                     (conj booms
                           (let [angle (rand TAU)
                                 r (rand player-size)]
                             {:x (+ @(:x other-player) (* r (Math/sin angle)))
                              :y (+ @(:y other-player) (* r (Math/cos angle)))
                              :c (rand-nth [Color/WHITE Color/RED Color/YELLOW Color/LIGHT_GRAY Color/ORANGE])})))]

        (.setStroke g (BasicStroke. 1))
        (line x y (+ x (* gravity-x 100)) (+ y (* gravity-y 100)) Color/BLUE)

        (.setStroke g (BasicStroke. 10 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
        (doseq [[colour [[x1 y1] [x2 y2]]] (map vector trail-colours (partition 2 1 trail))]
          (when x1
            (line x1 y1 x2 y2 colour)))

        (when-not boom?'
          (circle x y bullet-size Color/BLUE))

        (run! draw-player [player other-player])

        (doseq [[idx {:keys [x y c]}] (reverse (map-indexed vector booms))]
          (circle x y idx (rgba-lerp c (:bg-colour player) (/ (count booms) 150))))

        (q)
        (Thread/sleep 10)
        (when (and #_(< y (+ bullet-size height))
                   (pos? limit')
                   (not (:quit? @(:egroeg player))))
          (recur x' y' dx' dy' ddx' ddy'
                 limit'
                 (take trail-length (cons (when-not boom?' [x' y']) trail))
                 boom?' booms'))
        )))

  ;(Thread/sleep 3000)
  )


(defn clear-for-george [player]
  (.setColor g (:bg-colour player))
  (.fillRect g 0 0 width height))

(defn game []
  (let [players [{:name      "Player 1"
                  :x         (atom (- width 50 (rand-int (* width 1/3))))
                  :y         (atom (+ (rand-int (* height 1/2)) 100))
                  :score     (atom 0)
                  :colour    (Color. 200 0 230)
                  :bg-colour Color/RED
                  :egroeg    (atom {:angle 0
                                    :power 50})}
                 {:name      "Player 2"
                  :x         (atom (+ 50 (rand-int (* width 1/3))))
                  :y         (atom (+ (* height 1/2) (rand-int (* height 1/2)) -100))
                  :score     (atom 0)
                  :colour    Color/ORANGE
                  :bg-colour Color/GREEN
                  :egroeg    (atom {:angle 0
                                    :power 50})}]]
    (loop [next-players (cycle players)]

      (let [current-player (first next-players)
            other-player (second next-players)]

        (when (:dead? @(:egroeg current-player))
          (println "Reborn!!!")
          (reset! (:egroeg current-player) {:angle 0
                                            :power 50})
          (reset! (:y current-player) (+ (rand-int (- (- height 100) 100)) 100))
          (reset! (:x current-player) (+ (rand-int (- (- width 100) 100)) 100))
          (swap! (:score other-player) inc))

        (clear-for-george current-player)
        (run! draw-player players)
        (q)

        (turn current-player other-player))

      ;(Thread/sleep 1000)
      (when-not (some :quit? (map deref (map :egroeg players)))
        (recur (rest next-players))))

    (clear-for-george {:bg-colour Color/WHITE})
    (q)))

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
                            [(range -25 -2 2) Color/WHITE]
                            [(range 25 2 -2) Color/BLACK])]
    {:p         p
     :size      (first lifecycle)
     :lifecycle lifecycle
     :color     color}))

(defn initial-state [width height player-names player-colors]
  {:width          width
   :height         height
   :players        (mapv (fn [player-name player-color]
                           {:label (subs player-name 0 (min 20 (count player-name)))
                            :p     (random-location width height []) ; TODO: anti-targets!
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
   :black-holes    [(new-black-hole [(* width 1/2) (* height 1/2)])]})

(defn render-players [{:keys [players current-player bg-color current-phase]}]
  (doseq [[idx {:keys [p color angle power]}] (map-indexed vector players)]
    (let [pf (+ player-radius (* gun-length (/ power 80)))
          aiming? (and (= current-phase :aiming) (= idx current-player))]
      (when aiming?
        (circle p (inc (inc (+ player-radius gun-length))) bg-color)
        (.setStroke g (BasicStroke. 1))
        (circle p (+ player-radius gun-length) (rgba-lerp Color/WHITE bg-color 0.75))
        (outlined-circle p pf (rgba-lerp Color/BLACK bg-color 0.75)))
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
              (Color. 255 (+ 95 (* power 2)) 0)))

      (.setStroke g (BasicStroke. 3))
      (outlined-circle p player-radius color)

      (when aiming?
        (.setColor g Color/BLACK)
        (.setFont g (Font. Font/SANS_SERIF Font/BOLD 24))
        (let [bounds (-> g .getFontMetrics (.getStringBounds (str power) g))
              [x y] p]
          (.drawString g (str power)
                       (int (- x (.getCenterX bounds)))
                       (int (inc (- y (.getCenterY bounds))))))))))

(defmulti tick :current-phase)

(defn render-black-holes [{:keys [black-holes]}]
  (doseq [{:keys [p size]} black-holes]
    (let [[x y] p]
      (circle x y (abs size) (if (pos? size)
                               Color/BLACK
                               Color/WHITE)))))

(defmacro destr [java-object & accessors]
  (let [fields (map (fn [accessor]
                    (list accessor java-object)) accessors)]
    `[~@fields]))

(defn render-scores [{:keys [players]}]
  (.setFont g (Font. Font/SANS_SERIF Font/BOLD 24))
  (doseq [[idx {:keys [score color label]}] (map-indexed vector players)]
    (let [corner-diameter 28
          v-inset 5
          h-inset 10
          xc (+ (* width 1/5) (* width 2/5 (/ idx (dec (count players)))))
          yc 50
          text (str label ": " score)
          [width height center-x center-y] (-> g .getFontMetrics (.getStringBounds text g)
                                               (destr .getWidth .getHeight .getCenterX .getCenterY))
          width (+ width h-inset h-inset)
          height (+ height v-inset v-inset)
          left (- xc (/ width 2))
          top (- yc (/ height 2))]
      ;(circle xc yc 25 color)
      (.setColor g color)
      (.fillRoundRect g left top width height corner-diameter corner-diameter)
      (.setStroke g (BasicStroke. 1))
      (.setColor g Color/BLACK)
      (.drawRoundRect g left top width height corner-diameter corner-diameter)
      (.drawString g text
                   (int (- xc center-x))
                   (int (- yc center-y))))))

(defn await-command [{:keys [current-player] :as state}]
  (if-let [key-code (async/<!! key-chan)]
    (case key-code
      37 (update-in state [:players current-player :angle] dec)
      38 (update-in state [:players current-player :power] increase-power)
      33 (update-in state [:players current-player :angle] increase-angle)
      34 (update-in state [:players current-player :angle] decrease-angle)
      39 (update-in state [:players current-player :angle] inc)
      40 (update-in state [:players current-player :power] decrease-power)
      10 (assoc state :current-phase :firing)
      27 (assoc state :current-phase :exit!)
      (do (println key-code)
          state))
    (assoc state :current-phase :exit!)))

(defmethod tick :aiming [state]
  (render-players state)
  (render-black-holes state)
  (render-scores state)
  (q)
  (await-command state))

(defn clear [{:keys [bg-color]}]
  (.setColor g bg-color)
  (.fillRect g 0 0 width height)
  (q))

(def bullet-power-factor 0.2)

(defn but-nth [n coll]
  (concat (take n coll)
          (drop (inc n) coll)))

(defmethod tick :firing [{:keys [current-player bg-color players] :as state}]
  (let [{:keys [angle power color p]} (get-in state [:players current-player])
        trail-config (map (fn [p]
                            {:color (rgb-lerp color Color/DARK_GRAY p)
                             :width (- (* 2 bullet-size) (* p (dec (* 2 bullet-size))))})
                          (range 0 1.0001 1/25))
        bullet-colors (map (fn [p]
                             (rgb-lerp Color/DARK_GRAY color p))
                           (range 0 1.0001 1/30))]
    (assoc state :projection {:targets         (but-nth current-player (map-indexed vector players))
                              :p               (map +
                                                    p
                                                    [(* (+ player-radius gun-length) (Math/sin (degrees->radians angle)))
                                                     (* (+ player-radius gun-length) (Math/cos (degrees->radians angle)) -1)])
                              :dp              [(* power (Math/sin (degrees->radians angle)) bullet-power-factor)
                                                (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)]
                              :trail-config trail-config
                              ;:trail-colors    trail-colors
                              :bullet-colors   (cycle (concat bullet-colors (reverse bullet-colors)))
                              :limit           10000
                              :trail           []
                              :enemy-hit-index nil
                              :booms           nil}
                 :current-phase :projecting)))

(def bullet-size 4)

(defmethod tick :projecting [{:keys [projection black-holes bg-color width height players] :as state}]
  (let [{:keys [targets p dp trail-config bullet-colors limit trail enemy-hit-index booms]} projection
        trail-length (count trail-config)
        [x y] p
        ddp (reduce (fn [acc black-hole]
                      (let [[bh-x bh-y] (:p black-hole)]
                        (map - acc (calculate-gravity bh-x bh-y (:size black-hole) x y))))
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
                 (conj booms
                       (let [angle (rand TAU)
                             r (rand player-size)]
                         {:x (+ (get-in players [enemy-hit-index' :p 0]) (* r (Math/sin angle)))
                          :y (+ (get-in players [enemy-hit-index' :p 1]) (* r (Math/cos angle)))
                          :c (rand-nth [Color/WHITE Color/RED Color/YELLOW Color/LIGHT_GRAY Color/ORANGE])})))
        trail' (take trail-length (cons (when-not enemy-hit-index' p') trail))]

    (when-not enemy-hit-index'
      (circle p (inc bullet-size) bg-color))

    ;(.setStroke g (BasicStroke. 2 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))

    (doseq [[{:keys [color width]} [[x1 y1] [x2 y2]]] (map vector trail-config (partition 2 1 trail))
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
    (doseq [[idx {:keys [x y c]}] (reverse (map-indexed vector booms))]
      (circle x y (+ idx player-radius) (rgba-lerp c (set-alpha c 0) (/ (count booms) 150))))

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
                   :enemy-hit-index enemy-hit-index'))))

(defn process-black-holes [black-holes]
  (for [{:keys [lifecycle] :as hole} black-holes
        :when (seq lifecycle)]
    (assoc hole :size (first lifecycle)
                :lifecycle (rest lifecycle))))

(defmethod tick :progressing [{:keys [players enemy-hit-index current-player black-holes] :as state}]
  (while (async/poll! key-chan))
  (-> (if enemy-hit-index
        (-> state
            (update-in [:players current-player :score] inc)
            (assoc-in [:players enemy-hit-index :p] (random-location width height
                                                                     (concat (but-nth enemy-hit-index players)
                                                                             black-holes)))
            (update :black-holes process-black-holes)
            (update :black-holes conj (new-black-hole (get-in players [enemy-hit-index :p]))))
        state)
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

(comment
  (future
    (try
      (x)
      (catch Exception e
        (.printStackTrace e)))))
