(defclass point ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (color :reader color :initarg :color)))

(defun make-point (x y &optional (color :black))
  (make-instance 'point :x x :y y :color color))

(defclass size ()
  ((w :reader w :initarg :w)
   (h :reader h :initarg :h)))

(defun make-size (w h)
  (make-instance 'size :w w :h h))

(defclass rectangle (point size) ())

(defun make-rectangle (x y w h &optional (color :black))
  (make-instance 'rectangle :x x :y y :w w :h h :color color))

(defmethod center ((movable point)) movable)

(defmethod center ((movable rectangle))
  (make-point (+ (x movable) (/ (w movable) 2))
              (+ (y movable) (/ (h movable) 2))))

(defun move (movable dx dy)
  (with-new (x y) movable
    (incf x dx)
    (incf y dy)))

(defmethod rotate (movable)
  (rotate-around movable (center movable)))

(defmethod rotate-around (movable origin)
  (let ((rx (- (x movable) (x origin)))
        (ry (- (y movable) (y origin))))
    (with-new (x y) movable
      (setf x (+ (- ry) (x origin))
            y (+ rx (y origin))))))

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
    
    (with-new (x y w h) movable
      (setf x left
            y top
            w (- right left)
            h (- bot top)))))

(defun scale (scalable value)
  (with-new (w h) scalable
    (setf w (* value w)
          h (* value h))))

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
      (with-new (x y w h) base
        (setf x left y top
              w (- right left)
              h (- bot top))))))

(defmethod print-object ((o point) stream)
  (format stream "Point(~a, ~a, ~a)" (x o) (y o) (color o)))

(defmethod print-object ((o size) stream)
  (format stream "Size(~a, ~a)" (w o) (h o)))

(defmethod print-object ((o rectangle) stream)
  (format stream "Rectangle(~a, ~a, ~a, ~a, ~a)" (x o) (y o) (w o) (h o) (color o)))


(defun main ()
  (let* ((p (make-point 2 2 :yellow))
         (p2 (move p 1 2))
         (p3 (rotate p))
         (p4 (rotate-around p (make-point 0 0)))

         (s (make-size 4 6))
         (s2 (scale s 3))
         (s3 (fit-into s (make-size 3 3)))

         (r (make-rectangle 2 2 3 4 :green))
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