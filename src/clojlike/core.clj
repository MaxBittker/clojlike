(ns clojlike.core
  (:gen-class))

(require '[lanterna.screen :as s])

(def screen-size (ref [80 24]))

(defn handle-resize [cols rows]
  (dosync (ref-set screen-size [cols rows])))

(def scr (s/get-screen :swing {:resize-listener handle-resize}))
; (def scr (s/get-screen ))
(declare startScreen)

(defn reflect
  [x y]
  (let [[w h] (map dec @screen-size)]
      [[x y]
       [x (+ h (* -1 y))]
       [(+ w (* -1 x)) y]
       [(+ w (* -1 x)) (+ h (* -1 y))]]))

(defn drawPixel
  [[x y] ch]
  (s/put-string scr x y ch))

(defn wipe
  []
  (let [[w h] @screen-size]
    (doseq [y (range h)] (s/put-string scr 0 y (apply str (repeat w " "))))))

(defn drawDot
    [x y o]
    (let [ch (str (char (mod (+ o (* y (quot (first @screen-size) 2) x)) 60000)))]
        (doseq [side (reflect x y)] (drawPixel side ch))))


(defn newFlake
  []
  [(rand-int (first @screen-size)) -10 (rand-int 60000) (+ 3(rand-int 7))])

(defn drawComet
  [[x y state lng]]
  (loop [o 0]
      (s/put-string scr x (+ o y) (str (char (+ state o))))
      (if (> o lng)
       true
       (recur (+ o 1)))))

(defn checkCollision
  [[cx cy st lng] px py]
  (and (= px cx) (< cy py) (> (+ cy lng) py)))

(defn game
    [lvl]
    (loop [[x y] [0 (quot (second @screen-size) 2)]
           comets '()]
      (wipe)
      (let [comets (filter (fn [[cx cy & rest]] (< cy (second @screen-size)))
                      (map (fn [[cx cy st lng]] [cx (inc cy) (inc st) lng])
                        (concat comets (take lvl (repeatedly newFlake)))))
            hit (empty? (filter (fn [c] (checkCollision c x y)) comets))]
        (doseq [c comets] (drawComet c))
        (s/put-string scr 0 0 (str "level: " lvl))
        (if hit
            (s/put-string scr x y "@")
            (s/put-string scr x y "X" {:bg :red}))
        (s/redraw scr)
        (Thread/sleep 100)
        (let [key (s/get-key scr)]
          (dotimes [d 5] (s/get-key scr))
          (if (not hit)
           (startScreen)
           (if (= x (first @screen-size))
             (game (inc lvl))
             (recur (map + [x y] (case key
                                    :up [0 -1]
                                    :left [-1 0]
                                    :down [0 1]
                                    :right [1 0]
                                    [0 0])) comets)))))))

(defn frame
  [w h o]
  (doseq [x (range (quot w 2)) y (range (quot h 2))] (drawDot x y o))
  (s/put-string scr
    (quot (- w 19) 2)
    (mod (+ (quot o 15) (quot h 2)) h)
    "WELCOME TO CONSORTIUM" {:fg :black :bg :yellow})
  (s/put-string scr
    (quot (- w 10) 2)
    (mod (+  1 (quot o 15) (quot h 2)) h)
    "PRESS ENTER" {:fg :black :bg :yellow})
  (s/redraw scr)
  (if (= :enter (s/get-key scr))
    (game 1)
    (let [[w h ](s/get-size scr)]
      (recur w h (inc o)))))

(defn startScreen
    []
    (s/start scr)
    (let [[w h ](s/get-size scr)]
      (frame w h 1)))

(startScreen)
