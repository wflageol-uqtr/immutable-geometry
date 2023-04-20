(ns geometry)

(derive ::rectangle ::point)
(derive ::rectangle ::size)

(defn point [x y]
      {:shape ::point :x x :y y})

(defn size [w h]
      {:shape ::size :w w :h h})

(defn rectangle [x y w h]
      {:shape ::rectangle :x x :y y :w w :h h})

(defmulti move (fn [moveable _ _] (moveable :shape)))
(defmethod move ::point [moveable dx dy]
      (assoc moveable
             :x (+ (moveable :x) dx)
             :y (+ (moveable :y) dy)))

(defmulti center :shape)
(defmethod center ::point [point] point)
(defmethod center ::rectangle [rect]
           (point (Math/floor (double (+ (rect :x) (/ (rect :w) 2))))
                  (Math/floor (double (+ (rect :y) (/ (rect :h) 2))))))

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

                (rectangle left top (- right left) (- bot top))))

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
               (rectangle left top (- right left) (- bot top)))))


(defn main []
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

           (println p)
           (println p2)
           (println p3)
           (println p4)

           (println s)
           (println s2)
           (println s3)

           (println r)
           (println r2)
           (println r3)
           (println r4)
           (println r5)
           (println r6)
           (println r7)))