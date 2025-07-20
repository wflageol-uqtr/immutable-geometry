(defclass point ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)))

(defun make-point (x y)
  (make-instance 'point :x x :y y))

(defclass size ()
  ((w :reader w :initarg :w)
   (h :reader h :initarg :h)))

(defun make-size (w h)
  (make-instance 'size :w w :h h))

(defclass rectangle (point size) ())

(defun make-rectangle (x y w h)
  (make-instance 'rectangle :x x :y y :w w :h h))

(defmethod update-movable ((movable point) x y)
  (make-point x y))

(defmethod update-movable ((movable rectangle) x y)
  (make-rectangle x y (w movable) (h movable)))

(defmethod center ((movable point)) movable)

(defmethod center ((movable rectangle))
  (make-point (+ (x movable) (/ (w movable) 2))
              (+ (y movable) (/ (h movable) 2))))

(defmethod move (movable dx dy)
  (update-movable movable
                  (+ dx (x movable))
                  (+ dy (y movable))))

(defmethod rotate (movable)
  (rotate-around movable (center movable)))

(defmethod rotate-around (movable origin)
  (let ((x (- (x movable) (x origin)))
        (y (- (y movable) (y origin))))
    (update-movable movable
                    (+ (- y) (x origin))
                    (+ x (y origin)))))

(defmethod rotate-around ((movable rectangle) origin)
  (let* ((top-left (make-point (x movable) (y movable)))
         (top-right (make-point (+ (x movable) (w movable)) (y movable)))
         (bot-left (make-point (x movable) (+ (y movable) (h movable))))

         (p1 (rotate-around top-left origin))
         (p2 (rotate-around top-right origin))
         (p3 (rotate-around bot-left origin))

         (y-list (list (y p1) (y p2) (y p3)))
         (x-list (list (x p1) (x p2) (x p3)))

         (top (apply #'min y-list))
         (bot (apply #'max y-list))
         (left (apply #'min x-list))
         (right (apply #'max x-list)))
    (make-rectangle left top
                    (- right left) (- bot top))))

(defmethod update-scalable ((scalable size) w h)
  (make-size w h))

(defmethod update-scalable ((scalable rectangle) w h)
  (make-rectangle (x scalable) (y scalable) w h))

(defmethod scale (scalable value)
  (update-scalable scalable
                   (* value (w scalable))
                   (* value (h scalable))))

(defmethod fit-into (scalable target)
  (scale scalable
         (min (/ (h target) (h scalable))
              (/ (w target) (w scalable)))))


(defun left (rectangle) (x rectangle))
(defun right (rectangle) (+ (x rectangle) (w rectangle)))
(defun top (rectangle) (y rectangle))
(defun bottom (rectangle) (+ (y rectangle) (h rectangle)))

(defmethod intersectp ((base rectangle) target)
  (or (< (left target) (right base))
      (< (top target) (bottom base))
      (< (left base) (right target))
      (< (top base) (bottom target))))

(defmethod intersect ((base rectangle) target)
  (when (intersectp base target)
    (let ((top (max (top base) (top target)))
          (bot (min (bottom base) (bottom target)))
          (left (max (left base) (left target)))
          (right (min (right base) (right target))))
      (make-rectangle left top
                      (- right left) (- bot top)))))


(defmethod print-object ((o point) stream)
  (format stream "Point(~a, ~a)" (x o) (y o)))

(defmethod print-object ((o size) stream)
  (format stream "Size(~a, ~a)" (w o) (h o)))

(defmethod print-object ((o rectangle) stream)
  (format stream "Rectangle(~a, ~a, ~a, ~a)" (x o) (y o) (w o) (h o)))


(defun main ()
  (let* ((p (make-point 2 2))
         (p2 (move p 1 2))
         (p3 (rotate p))
         (p4 (rotate-around p (make-point 0 0)))

         (s (make-size 4 6))
         (s2 (scale s 3))
         (s3 (fit-into s (make-size 3 3)))

         (r (make-rectangle 2 2 3 4))
         (r2 (move r 1 2))
         (r3 (rotate r))
         (r4 (rotate-around r (make-point 0 0)))

         (r5 (scale r 5))
         (r6 (fit-into r (make-size 8 8)))

         (r7 (intersect r (make-rectangle 4 3 3 2))))

    (format t "~a~%" p)
    (format t "~a~%" p2)
    (format t "~a~%" p3)
    (format t "~a~%" p4)

    (format t "~a~%" s)
    (format t "~a~%" s2)
    (format t "~a~%" s3)

    (format t "~a~%" r)
    (format t "~a~%" r2)
    (format t "~a~%" r3)
    (format t "~a~%" r4)
    (format t "~a~%" r5)
    (format t "~a~%" r6)
    (format t "~a~%" r7)))
