(ns core
  (:require [see :as see])
  (:import [java.awt.image BufferedImage]
           [java.awt Graphics2D Color BasicStroke Font]))


(println "Hello")

(def TAU (* Math/PI 2))

(def width 1500)
(def height 900)
(def player-size 40)
(def bullet-size 10)

(def current-player-state (atom nil))

(defn increase-power [power]
  (min (inc power) 100))

(defn decrease-power [power]
  (max (dec power) -120))

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

(defonce image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
(defonce q (see/see image :key-handler-fn key-handler-fn))
(defonce g ^Graphics2D (.getGraphics image))

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

(defn circle [x y size colour]
  (.setColor g colour)
  (.fillArc g
            (int (- x (/ size 2)))
            (int (- y (/ size 2)))
            (int size) (int size) 0 360))

(defn outlined-circle [x y size colour]
  (.setColor g colour)
  (.fillArc g
            (int (- x (/ size 2)))
            (int (- y (/ size 2)))
            (int size) (int size) 0 360)
  (.setColor g Color/BLACK)
  (.drawArc g
            (int (- x (/ size 2)))
            (int (- y (/ size 2)))
            (int size) (int size) 0 360))

(defn line [x1 y1 x2 y2 colour]
  (.setColor g colour)
  (.drawLine g (int x1) (int y1) (int x2) (int y2)))

(defn degrees->radians [degrees]
  (-> degrees (/ 360) (* TAU)))

(defn draw-player [player]
  (let [gun-length 40
        power (:power @(:egroeg player))]

    (circle (:x player)
            @(:y player)
            (inc (inc (+ player-size gun-length)))
            (:bg-colour player))

    (.setStroke g (BasicStroke. 1))
    (outlined-circle (:x player)
                     @(:y player)
                     (+ player-size (* 1/4 gun-length))
                     (rgb-lerp Color/BLACK (:bg-colour player) 0.75))

    (.setStroke g (BasicStroke. 10 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
    (let [angle (:angle @(:egroeg player))]
      (line (:x player)
            @(:y player)
            (+ (:x player) (* gun-length (Math/sin (degrees->radians angle))))
            (+ @(:y player) (* gun-length (Math/cos (degrees->radians angle)) -1))
            Color/BLACK))

    (.setStroke g (BasicStroke. 3))
    (outlined-circle (:x player)
                     @(:y player)
                     player-size
                     (:colour player))

    (.setColor g Color/BLACK)
    (.setFont g (Font. Font/SANS_SERIF Font/BOLD (if (= power 100) 18 24)))
    (let [bounds (-> g .getFontMetrics (.getStringBounds (str power) g))]
      (.drawString g (str power)
                   (int (- (:x player) (.getCenterX bounds)))
                   (int (- @(:y player) (.getCenterY bounds)))))))

(defn hit-detected-g [x1 y1 x2 y2 distance]
  (let [a (- x1 x2)
        b (- y1 y2)
        c (Math/sqrt (+ (* a a) (* b b)))]
    (< c distance)))

(defn player-choosing-angle [player]

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
        (q)
        ;(println @@current-player-state)
        ;(println @state)
        ;(println player)

        (Thread/sleep 100)
        (recur))
      )
    ;(println "loop ended")
    ))

(defn turn [player other-player]
  (player-choosing-angle player)
  (let [angle (:angle @(:egroeg player))
        power (:power @(:egroeg player))
        bullet-power-factor 0.2
        trail-colours (map (fn [p]
                             (rgb (+ p (.getRed (:bg-colour player)))
                                  (+ p (.getGreen (:bg-colour player)))
                                  (+ p (.getBlue (:bg-colour player)))))
                           (range 250 -1 -10))
        trail-length (count trail-colours)]
    (loop [x (:x player)
           y @(:y player)
           dx (* power (Math/sin (degrees->radians angle)) bullet-power-factor)
           dy (* power (Math/cos (degrees->radians angle)) -1 bullet-power-factor)
           ddx 0
           ddy 0.1
           limit 1000
           trail []
           boom? false
           booms nil]
      (let [x' (+ x dx)
            y' (+ y dy)
            dx' (+ dx ddx)
            dy' (+ dy ddy)
            limit' (dec (if (and (< y (+ bullet-size height))
                                 (< 0 x width))
                          limit
                          (min limit 200)))
            boom?' (or boom?
                       (when (hit-detected-g (:x other-player) @(:y other-player)
                                             x y
                                             (/ (+ bullet-size player-size) 2))
                         (println "BOOM!!!")
                         (swap! (:egroeg other-player) assoc :dead? true)
                         true))
            booms' (when boom?
                     (conj booms
                           (let [angle (rand TAU)
                                 r (rand player-size)]
                             {:x (+ (:x other-player) (* r (Math/sin angle)))
                              :y (+ @(:y other-player) (* r (Math/cos angle)))
                              :c (rand-nth [Color/WHITE Color/RED Color/YELLOW Color/LIGHT_GRAY Color/ORANGE])})))]

        (.setStroke g (BasicStroke. 10 BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER))
        (doseq [[colour [[x1 y1] [x2 y2]]] (map vector trail-colours (partition 2 1 trail))]
          (when x1
            (line x1 y1 x2 y2 colour)))

        (when-not boom?'
          (circle x y bullet-size Color/BLUE))

        (run! draw-player [player other-player])

        (doseq [[idx {:keys [x y c]}] (reverse (map-indexed vector booms))]
          (circle x y idx (rgb-lerp c (:bg-colour player) (/ (count booms) 150))))

        (q)
        (Thread/sleep 10)
        (when (and #_(< y (+ bullet-size height))
                   (pos? limit')
                   (not (:quit? @(:egroeg player))))
          (recur x' y' dx' dy' ddx ddy
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
                  :x         (- width 50 (rand-int (* width 1/3)))
                  :y         (atom (+ (rand-int (* height 1/2)) 100))
                  :colour    (Color. 200 0 230)
                  :bg-colour Color/RED
                  :egroeg    (atom {:angle 0
                                    :power 50})}
                 {:name      "Player 2"
                  :x         (+ 50 (rand-int (* width 1/3)))
                  :y         (atom (+ (* height 1/2) (rand-int (* height 1/2)) -100))
                  :colour    Color/ORANGE
                  :bg-colour Color/GREEN
                  :egroeg    (atom {:angle 0
                                    :power 50})}]]
    (loop [next-players (cycle players)]

      (let [current-player (first next-players)]

        (when (:dead? @(:egroeg current-player))
          (println "Reborn!!!")
          (reset! (:egroeg current-player) {:angle 0
                                            :power 50})
          (reset! (:y current-player) (+ (rand-int (- (- height 100) 100)) 100)))

        (clear-for-george current-player)
        (run! draw-player players)
        (q)

        (turn current-player (second next-players)))

      ;(Thread/sleep 1000)
      (when-not (some :quit? (map deref (map :egroeg players)))
        (recur (rest next-players))))

    (clear-for-george {:bg-colour Color/WHITE})
    (q)))