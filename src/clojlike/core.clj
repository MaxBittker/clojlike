(ns clojlike.core
  (:gen-class))

(require '[lanterna.screen :as s])
(def screen-size (ref [80 24]))

(defn handle-resize [cols rows]
  (dosync (ref-set screen-size [cols rows])))

(def scr (s/get-screen :swing {:resize-listener handle-resize}))

(declare startScreen)
(declare snakeFrame)

(defn reflect
  "takes a coordinate and returns its 4 reflections"
  [x y]
  (let [[w h] (map dec @screen-size)]
      [[x y]
       [x (+ h (* -1 y))]
       [(+ w (* -1 x)) y]
       [(+ w (* -1 x)) (+ h (* -1 y))]]))

(defn safeRedraw
  "wraps redraw in a try catch for when screen size has changed"
  []
  (try
    (s/redraw scr)
    (catch Exception e (str "caught exception: " (.getMessage e)))))

(defn drawPixel
  "put a pixel to the buffer"
  [[x y] ch]
  (s/put-string scr x y ch))

(defn wipe
  "sets screen to all spaces"
  []
  (let [[w h] @screen-size]
    (doseq [y (range h)] (s/put-string scr 0 y (apply str (repeat w " "))))))

(defn drawGlyph
    "draws a unicode glyph on 4 symetrical locations"
    [x y o]
    (let [ch (str (char (mod (+ o (* y (quot (first @screen-size) 2) x)) 64000)))]
        (doseq [side (reflect x y)] (drawPixel side ch))))

(defn newComet
  "Build a new comet"
  []
  [(rand-int (first @screen-size)) -10 (rand-int 64000) (+ 3(rand-int 7))])

(defn drawComet
  "draw a comet (including it's tail)"
  [[x y state lng]]
  (loop [o 0]
      (s/put-string scr x (+ o y) (str (char (+ state o))))
      (if (> o lng)
       true
       (recur (+ o 1)))))

(defn checkCollision
  "check collision between a comet and a player"
  [[cx cy st lng] px py]
  (and (= px cx) (< cy py) (> (+ cy lng) py)))

(defn tickComets
  "add new comets, then for each commet, move it, age it, and filter dead ones out"
  [x y lvl comets]
  (filter (fn [[cx cy & rest]] (< cy (second @screen-size)))
          (map (fn [[cx cy st lng]] [cx (inc cy) (inc st) lng])
              (concat comets (take lvl (repeatedly newComet))))))

(defn game
   "game loop for comet game"
    [lvl]
    (loop [[x y] [0 (quot (second @screen-size) 2)]
           comets '()]
      (wipe)
      (let [comets (tickComets x y lvl comets)
            hit (empty? (filter (fn [c] (checkCollision c x y)) comets))]
        (doseq [c comets] (drawComet c))
        (s/put-string scr 0 0 (str "level: " lvl))
        (if hit
            (s/put-string scr x y "@")
            (startScreen))
        (safeRedraw)
        (Thread/sleep 100)
        (let [key (s/get-key scr)]
          (dotimes [d 5] (s/get-key scr))
          (if (= x (first @screen-size))
             (game (inc lvl))
             (recur (map + [x y] (case key
                                    :up [0 -1]
                                    :left [-1 0]
                                    :down [0 1]
                                    :right [1 0]
                                    [0 0])) comets))))))

(defn frame
  "start screen per-frame logic"
  [w h o]
  (doseq [x (range (quot w 2)) y (range (quot h 2))] (drawGlyph x y o))
  (s/put-string scr
    (quot (- w 19) 2)
    (mod (+ (quot o 15) (quot h 2)) h)
    "WELCOME TO CONSORTIUM" {:fg :black :bg :yellow})
  (s/put-string scr
    (quot (- w 10) 2)
    (mod (+  1 (quot o 15) (quot h 2)) h)
    "PRESS C or S" {:fg :black :bg :yellow})
  (safeRedraw)
  (case  (s/get-key scr)
    \c (game 1)
    \s (snakeFrame)
    (let [[w h ](s/get-size scr)]
      (recur w h (inc o)))))

(defn startScreen
    "initialize screen and start menu"
    []
    (s/start scr)
    (let [[w h ](s/get-size scr)]
      (frame w h 1)))

(defn drawSegment
  "draw one snake segment"
  [[x y]]
  (s/put-string scr x y (str (char (rand-int 64000)))))

(defn checkCollisionSnake
  "check collision between head and a snake segment"
  [[sx sy] [px py]]
  (and (= sx px) (= sy py)))


(defn newCoin
  "create a new random point"
  []
  [(rand-int (first @screen-size)) (rand-int (second @screen-size))])

(defn snakeFrame
  "snake game loop"
  []
  (s/start scr)
  (loop [snake '([10 10])
         dot   (newCoin)
         dir     [1 0]
         len     5]
   (wipe)
   (drawSegment dot)
   (doseq [seg snake] (drawSegment seg))
   (safeRedraw)
   (dotimes [d 5] (s/get-key scr))
   (Thread/sleep 100)
   (let [newdir (case (s/get-key scr)
                   :up [0 -1]
                   :left [-1 0]
                   :down [0 1]
                   :right [1 0]
                   dir)
         adjdir  (if (= [0 0](map + newdir dir))
                    dir
                    newdir)
         snake (cons (map + (first snake) adjdir) snake)
         hit (empty? (filter (fn [segment]
                                (checkCollisionSnake segment (first snake)))
                            (rest snake)))
         ate (checkCollisionSnake dot (first snake))
         len (if ate (inc len) len)
         dot (if ate (newCoin) dot)]
    (if (not hit)
     (startScreen)
     (recur (take len snake) dot adjdir len)))))


; (startScreen)
