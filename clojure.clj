(ns geometry)

(derive ::rectangle ::point)
(derive ::rectangle ::size)

(defn point [x y]
      {:shape ::point :x x :y y})

(defn size [w h]
      {:shape ::size :w w :h h})

(defn rectangle [x y w h]
      {:shape ::rectangle :x x :y y :w w :h h})
      
(defmulti print-geo (fn [geo-object] (geo-object :shape)))
(defmethod print-geo ::point [point]
  (println (format "Point(%d, %d)" (point :x) (point :y))))
(defmethod print-geo ::size [size]
  (println (format "Size(%d, %d)" (size :w) (size :h))))
(defmethod print-geo ::rectangle [rect]
  (println (format "Rectangle(%d, %d, %d, %d)" (rect :x) (rect :y) (rect :w) (rect :h))))

(defmulti move (fn [moveable _ _] (moveable :shape)))
(defmethod move ::point [moveable dx dy]
      (assoc moveable
             :x (+ (moveable :x) dx)
             :y (+ (moveable :y) dy)))

(defmulti center :shape)
(defmethod center ::point [point] point)
(defmethod center ::rectangle [rect]
           (point (.intValue (Math/floor (double (+ (rect :x) (/ (rect :w) 2)))))
                  (.intValue (Math/floor (double (+ (rect :y) (/ (rect :h) 2)))))))

(defmulti rotate-around (fn [moveable _] (moveable :shape)))
(defmethod rotate-around ::point [point origin]
           (let [x (- (point :x) (origin :x))
                 y (- (point :y) (origin :y))]
                (assoc point
                       :x (+ (- y) (origin :x))
                       :y (+ x (origin :y)))))
(defmethod rotate-around ::rectangle [rect origin]
           (let [top-left (point (rect :x) (rect :y))
                 top-right (point (+ (rect :x) (rect :w)) (rect :y))
                 bot-left (point (rect :x) (+ (rect :y) (rect :h)))

                 p1 (rotate-around top-left origin)
                 p2 (rotate-around top-right origin)
                 p3 (rotate-around bot-left origin)

                 y-list [(p1 :y) (p2 :y) (p3 :y)]
                 x-list [(p1 :x) (p2 :x) (p3 :x)]

                 top (apply min y-list)
                 bot (apply max y-list)
                 left (apply min x-list)
                 right (apply max x-list)]
                 
                (assoc rect
                       :x left 
                       :y top 
                       :w (- right left) 
                       :h (- bot top))))

(defmulti rotate :shape)
(defmethod rotate ::point [point]
           (rotate-around point (center point)))

(defmulti scale (fn [scaleable _] (scaleable :shape)))
(defmethod scale ::size [size value]
           (assoc size
                  :h (Math/round (double (* (size :h) value)))
                  :w (Math/round (double (* (size :w) value)))))

(defmulti fit-into (fn [scaleable _] (scaleable :shape)))
(defmethod fit-into ::size [size target]
           (scale size (min (/ (target :h) (size :h))
                            (/ (target :w) (size :w)))))

(defn top [rect] (rect :y))
(defn left [rect] (rect :x))
(defn right [rect] (+ (rect :x) (rect :w)))
(defn bottom [rect] (+ (rect :y) (rect :h)))

(defn does-intersect [rect target]
      (or (< (left target) (right rect))
          (< (top target) (bottom rect))
          (< (left rect) (right target))
          (< (top rect) (bottom target))))

(defn intersect [rect target]
      (if (does-intersect rect target)
          (let [top (max (top rect) (top target))
                bot (min (bottom rect) (bottom target))
                left (max (left rect) (left target))
                right (min (right rect) (right target))]
               (assoc rect
                      :x left 
                      :y top 
                      :w (- right left) 
                      :h (- bot top)))))


(let [p (point 2 2)
      p2 (move p 1 2)
      p3 (rotate p)
      p4 (rotate-around p (point 0 0))

      s (size 4 6)
      s2 (scale s 3)
      s3 (fit-into s (size 3 3))

      r (rectangle 2 2 3 4)
      r2 (move r 1 2)
      r3 (rotate r)
      r4 (rotate-around r (point 0 0))

      r5 (scale r 5)
      r6 (fit-into r (size 8 8))

      r7 (intersect r (rectangle 4 3 3 2))]

     (print-geo p)
     (print-geo p2)
     (print-geo p3)
     (print-geo p4)

     (print-geo s)
     (print-geo s2)
     (print-geo s3)

     (print-geo r)
     (print-geo r2)
     (print-geo r3)
     (print-geo r4)
     (print-geo r5)
     (print-geo r6)
     (print-geo r7))