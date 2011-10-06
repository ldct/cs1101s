

;;; CS1101S 2006/2009 Semester I, Problem Set 2

;;;



(define viewport-size 512)



(define viewport-size1 (- viewport-size 1))



(require (lib "graphics.ss" "graphics"))

(require (lib "trace.ss"))



(open-graphics)



;;; USEFUL, SIMPLE, GENERAL PROCEDURES



;(define (compose f g)

;  (lambda (x)

;    (f (g x))))



(define (thrice f)

  (compose (compose f f) f))



; (define (identity t) t)



(define (repeated f n)

  (if (= n 0)

      identity

      (compose f (repeated f (- n 1)))))





;;; USEFUL NUMERICAL PROCEDURE



(define (square x) (* x x))





;;; USEFUL ANGLES

(define -pi (- pi))

(define pi/4 (/ pi 4))

(define 2pi (* 2 pi))





;;; Code for Curves



                                        ;Point = (Sch-Num X Sch-Num)



(define (make-point x y) (cons x y))



(define (x-of point) (car point))



(define (y-of point) (cdr point))



;Unit-Interval = {x: Sch-Num | 0 <= x <= 1}

;Curve = Unit-interval --> Point



;;SOME CURVES



(define (unit-circle t)

  (make-point (sin (* 2pi t))

              (cos (* 2pi t))))



(define (unit-line t)

  (make-point t 0))



(define (alternative-unit-circle t)

  (make-point (sin (* 2pi (square t)))

              (cos (* 2pi (square t)))))


; made available for Mission 6
(define (arc t)
  (make-point (sin (* pi t))
              (cos (* pi t))))


;Curve-Transform = (Curve --> Curve)



;;SOME CURVE-TRANSFORMS



(define (revert curve)

  (lambda (t) (curve (- 1 t))))



(define (rotate-pi/2 curve)

  (lambda (t)

    (let ((ct (curve t)))

      (make-point

       (- (y-of ct))

       (x-of ct)))))



                   ;;CONSTRUCTORS OF CURVE-TRANSFORMS



;;; TRANSLATE is of type (Sch-Num, Sch-Num --> Curve-Transform)



(define (translate x0 y0)               

  (lambda (curve)

    (lambda (t)

      (let ((ct (curve t)))

        (make-point (+ x0 (x-of ct))

                    (+ y0 (y-of ct)))))))



;;; ROTATE-AROUND-ORIGIN is of type (Sch-Num --> Curve-Transform)



(define (rotate-around-origin theta)

  (let ((cth (cos theta))

        (sth (sin theta)))

    (lambda (curve)

      (lambda (t)

        (let ((ct (curve t)))

          (let ((x (x-of ct))

                (y (y-of ct)))

            (make-point

             (- (* cth x) (* sth y))

             (+ (* sth x) (* cth y)))))))))



(define (deriv-t n)

  (let ((delta_t (/ 1 n)))

    (lambda (curve)

      (lambda (t)

        (let ((ct (curve t))

              (ctdelta (curve (+ t delta_t))))

          (make-point (/ (- (x-of ctdelta) (x-of ct))

                         delta_t)

                      (/ (- (y-of ctdelta) (y-of ct))

                         delta_t)))))))



(define (scale-x-y a b)

  (lambda (curve)

    (lambda (t)

      (let ((ct (curve t)))

        (make-point (* a (x-of ct))

                    (* b (y-of ct)))))))



(define (scale s)

  (scale-x-y s s))



;;; SQUEEZE-RECTANGULAR-PORTION translates and scales a curve

;;; so the portion of the curve in the rectangle

;;; with corners xlo xhi ylo yhi will appear in a display window

;;; which has x, y coordinates from 0 to 1.

;;; It is of type (Sch-Num,Sch-Num,Sch-Num,Sch-Num --> Curve-Transform).



(define (squeeze-rectangular-portion xlo xhi ylo yhi)

  (let ((width (- xhi xlo))

        (height (- yhi ylo)))

    (if (or (zero? width) (zero? height))

        (error "attempt to squeeze window to zero")

        (compose (scale-x-y (/ 1 width)

                            (/ 1 height))

                 (translate (- xlo) (- ylo))))))



;;; SQUEEZE-FULL-VIEW translates and scales a curve such that
;;; the ends are fully visible.
;;; It is very similar to the squeeze-rectangular-portion procedure
;;; only that that procedure does not allow the edges to be easily seen
(define (squeeze-full-view xlo xhi ylo yhi)
  (let ((width (- xhi xlo))

        (height (- yhi ylo)))

    (if (or (zero? width) (zero? height))

        (error "attempt to squeeze window to zero")

        (compose (scale-x-y (* 0.99 (/ 1 width))

                            (* 0.99 (/ 1 height)))

                 (translate (- (- xlo 0.01)) (- (- ylo 0.01)))))))


;;; PUT-IN-STANDARD-POSITION is a Curve-Transform.

;;; A curve is in "standard position" if it starts at (0,0) ends at (1,0).

;;; A curve is PUT-IN-STANDARD-POSITION by rigidly translating it so its

;;; start point is at the origin, then rotating it about the origin to put

;;; its endpoint on the x axis, then scaling it to put the endpoint at (1,0).

;;; Behavior is unspecified on closed curves (with start-point = end-point).



(define (put-in-standard-position curve)

  (let* ((start-point (curve 0))

         (curve-started-at-origin

          ((translate (- (x-of start-point))

                      (- (y-of start-point)))

           curve))

         (new-end-point (curve-started-at-origin 1))

         (theta (atan (y-of new-end-point) (x-of new-end-point)))

         (curve-ended-at-x-axis

          ((rotate-around-origin (- theta)) curve-started-at-origin))

         (end-point-on-x-axis (x-of (curve-ended-at-x-axis 1))))

    ((scale (/ 1 end-point-on-x-axis)) curve-ended-at-x-axis)))





                                        ;Binary-transform = (Curve,Curve --> Curve)



;;; CONNECT-RIGIDLY makes a curve consisting of curve1 followed by curve2.



(define (connect-rigidly curve1 curve2)

  (lambda (t)

    (if (< t (/ 1 2))

	(curve1 (* 2 t))

	(curve2 (- (* 2 t) 1)))))



;;; CONNECT-ENDS makes a curve consisting of curve1 followed by

;;;  a copy of curve2 starting at the end of curve1



;;;(define (connect-ends curve1 curve2) ...)



                            ;;FRACTAL CURVES



;;; GOSPERIZE is a Curve-Transform



(define (gosperize curve)

  (let ((scaled-curve ((scale (/ (sqrt 2) 2)) curve)))

    (connect-rigidly ((rotate-around-origin (/ pi 4)) scaled-curve)

                     ((translate .5 .5)

                      ((rotate-around-origin (/ -pi 4)) scaled-curve)))))





;;; GOSPER-CURVE is of type (Sch-Num --> Curve)



(define (gosper-curve level)

  ((repeated gosperize level) unit-line))




                        ;;DRAWING GOSPER CURVES



(define (show-connected-gosper level)

  ((draw-connected g1 200)

   ((squeeze-rectangular-portion -.5 1.5 -.5 1.5)

    (gosper-curve level))))





                         ;;PARAMETERIZED GOSPER



;;; PARAM-GOSPER is of type ((Sch-Num,(Int --> Sch-Num)) --> Curve)



(define (param-gosper level angle-at)

  (if (= level 0)

      unit-line

      ((param-gosperize (angle-at level))

       (param-gosper (- level 1) angle-at))))



(define (param-gosperize theta)

  (lambda (curve)

    (let ((scale-factor (/ (/ 1 (cos theta)) 2)))

      (let ((scaled-curve ((scale scale-factor) curve)))

        (connect-rigidly ((rotate-around-origin theta) scaled-curve)

                         ((translate .5 (* (sin theta) scale-factor))

                          ((rotate-around-origin (- theta)) scaled-curve)))))))




                         ;;DRAGONIZE




;;;; Code for drawing

;;;

;;; NOT FOR STUDENT READING -- uses continuation passing style for multiple

;;; value return



;;; Apply (DRAW-... WINDOW N) to a curve to compute N points on the curve

;;; and display those points with coordinates in WINDOW.



(define (draw-points-on window n)

  (let ((1/n (/ 1 n)))

    (lambda (curve)

      (define (iter count)

        (let ((t (* count 1/n)))

          (let ((ct (curve t)))

            (let ((x (x-of ct))

                  (y (y-of ct)))

              ((draw-pixel window)

               (make-posn 

                (* (+ x 0.0) viewport-size1)

                (* (- 1.0 y) viewport-size1)))

              (if (>= count n)

                  'done

                  (iter (+ count 1)))))))

      ((clear-viewport window))

      (iter 0))))



(define (draw-connected window n)

  (let ((1/n (/ 1 n)))

    (lambda (curve)

      (define (iter x-old y-old count)

        (let ((t (* count 1/n)))

          (let ((ct (curve t)))

            (let ((x-new (x-of ct))

                  (y-new (y-of ct)))

              ((draw-line window)

               (make-posn

                (* (+ x-old 0.0) viewport-size1)

                (* (- 1.0 y-old) viewport-size1))

               (make-posn

                (* (+ x-new 0.0) viewport-size1)

                (* (- 1.0 y-new) viewport-size1)))

              (if (>= count n)

                  'done

                  (iter x-new y-new (+ count 1)))))))

      ((clear-viewport window))

      (let ((c0 (curve 0)))

        (iter (x-of c0) (y-of c0) 1)))))





;;; Apply (DRAW-...-SQUEEZED-... WINDOW N) to a curve to squeeze it to exactly

;;; fit in WINDOW and then compute and display N points on the curve in WINDOW.



(define (draw-points-squeezed-to-window window n)

  (lambda (curve)    

    ((draw-points-on window n)

     (((corners curve n) squeeze-rectangular-portion) curve))))



(define (draw-connected-squeezed-to-window window n)

  (lambda (curve)    

    ((draw-connected window n)

     (((corners curve n) squeeze-rectangular-portion) curve))))


;; makes the curve fully visible by leaving space around the perimeter of the curve
(define (draw-connected-full-view window n)
  (lambda (curve)
    ((draw-connected window n)
    (((corners curve n) squeeze-full-view) curve))))



;;; CORNERS computes the max and min values of the x and y coordinates

;;; of n points on a given curve.

;;; It then applies a given procedure CORNERS-USER of type

;;; ((Sch-Num,Sch-Num,Sch-Num,Sch-Num) --> Typ)

;;; to the four corner values, where Typ may be any type.



;;; CORNERS is of type (Curve,Sch-Num) --> (((Sch-Num,Sch-Num,Sch-Num,Sch-Num) --> Typ) --> Typ), 



(define (corners curve n)

  (let ((1/n (/ 1 n)))

    (lambda (corners-user)

      ;;AUX calls receiver on corners of curve segment given by

      ;;parameter value t in the interval [1/count, 1]

      (define (aux count receiver)

        (let ((point (curve (* count 1/n))))

          (let ((xc (x-of point))

                (yc (y-of point)))

            (if (>= count n)

                (receiver xc xc yc yc)

                (aux (+ count 1)

                     (lambda (x- x+ y- y+)

                       (receiver

                        (min x- xc)

                        (max x+ xc)

                        (min y- yc)

                        (max y+ yc))))))))

      (aux 0 corners-user))))



;;; Initialisation codes 



(define (make-window width height dx dy)

  (newline)

  (display "make-window")

  (make-graphics-device 'win32 width height))





(define g1 false)

(define g2 false)

(define g3 false)

(define (setup-windows)

  (newline)

  (display "setup-windows")

;  (if (and g1 (graphics-device? g1))

;      (graphics-close g1))

  (begin (set! g1 (open-viewport "G1" viewport-size viewport-size)))

;	 (graphics-set-coordinate-limits g1 0.0 0.0 1.0 1.0)

;	 (graphics-operation g1 'set-window-name "Graphics: g1"))

  (newline)

  (display "graphics window 1 done")

;  (if (and g2 (graphics-device? g2))

;      (graphics-close g2))

  (begin (set! g2 (open-viewport "G2" viewport-size viewport-size)))

;	 (graphics-set-coordinate-limits g2 0.0 0.0 1.0 1.0)

;	 (graphics-operation g2 'set-window-name "Graphics: g2"))

  (newline)

  (display "graphics window 2 done")

;  (if (and g3 (graphics-device? g3))

;      (graphics-close g3))

  (begin (set! g3 (open-viewport "G3" viewport-size viewport-size)))

;	 (graphics-set-coordinate-limits g3 0.0 0.0 1.0 1.0)

;	 (graphics-operation g3 'set-window-name "Graphics: g3"))

  (newline)

  (display "graphics window 3 done"))



(define (setup)

  (newline)

  (display "setup")

  (setup-windows))



(begin

  (setup)

  (newline)

  "Ready to draw")

