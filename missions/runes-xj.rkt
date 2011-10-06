(define (round-off x)
  (floor (+ x 0.5)))

; Setup graphics

(require (lib "graphics.ss" "graphics"))
(require file/gif)

(define viewport-size 600) ; This is the height of the viewport.
(define spread 20)
(open-graphics)
(define active-hollusion null)
(define hollusion-thread null)

; Note that we have removed argument vp from clear, draw, and stereogram method
; since we now only have 1 viewport.
(define vp (open-viewport "ViewPort" (* 4/3 viewport-size) viewport-size))
(define lp (open-viewport "Left Port" (* 4/3 viewport-size) viewport-size))
(define rp (open-viewport "Right Port" (* 4/3 viewport-size) viewport-size))
(define (clear) (begin
                  (if (not (null? active-hollusion))
                      (begin
                        (active-hollusion 'kill)
                        (kill-thread hollusion-thread)
                        (set! active-hollusion null)
                        (set! hollusion-thread null)))
                  ((clear-viewport vp))
                  ((clear-viewport lp))
                  ((clear-viewport rp))))
                  			
; Vector arithmetic

(define (add-vect p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))

(define (scale-vect r p)
  (make-posn (* r (posn-x p))
             (* r (posn-y p))))


; A graphic object (painter) is drawn in a frame 
; made up of an origin and 2 vectors

(define (make-frame p0 p1 p2 z1 z2)
  (list p0 p1 p2 z1 z2))

(define (frame-orig f) (car f))
(define (frame-x f) (cadr f))
(define (frame-y f) (caddr f))
(define (frame-z1 f) (cadddr f))
(define (frame-z2 f) (car (cddddr f)))


; Show a painter in the unit frame

(define unit-frame 
  (make-frame (make-posn (* 1/6 viewport-size) 0)
              (make-posn viewport-size 0)
              (make-posn 0 viewport-size)
              0
              1))

(define (show painter)
  (painter vp unit-frame))

					
; Translate a point from the unit frame into a new frame

(define (transform-posn frame)
  (lambda (posn)
    (add-vect (frame-orig frame)
              (add-vect (scale-vect (/ (posn-x posn) viewport-size)
                                    (frame-x frame))
                        (scale-vect (/ (posn-y posn) viewport-size)
                                    (frame-y frame))))))

(define (translate-posn posn dist)
  (make-posn (+ (posn-x posn) dist) (posn-y posn)))
  

; Zero element of painting

(define (null-painter frame) 'nothing-to-do)

					
; Some useful predefined painters (page 16 in "Concrete Abstractions")

(define (rcross-bb vp frame)
  (let ((p1 (list (make-posn 0 0)
                  (make-posn (/ viewport-size 4) (/ viewport-size 4))
                  (make-posn (/ (* 3 viewport-size) 4) (/ viewport-size 4))
                  (make-posn (/ (* 3 viewport-size) 4) (/ (* 3 viewport-size) 4))
                  (make-posn viewport-size viewport-size)
                  (make-posn viewport-size 0)))
        (p2 (list (make-posn (/ viewport-size 4) (/ viewport-size 4))
                  (make-posn (/ viewport-size 4) (/ (* 3 viewport-size) 4))
                  (make-posn (/ (* 3 viewport-size) 4) (/ (* 3 viewport-size) 4)))))
    
    (if (list? vp) 
        (begin
          (for-each (lambda (port count) 
                      ((draw-solid-polygon port) (map (transform-posn frame) p1)
                                                 (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                    vp (build-list (length vp) values))
          (for-each (lambda (port count) 
                      ((draw-solid-polygon port) (map (transform-posn frame) p2)
                                                 (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                    vp (build-list (length vp) values)))
        (begin
          ((draw-solid-polygon vp) (map (transform-posn frame) p1)
                                   (make-posn 0 0)
                                   (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame)))
          ((draw-solid-polygon vp) (map (transform-posn frame) p2)
                                   (make-posn 0 0)
                                   (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame)))))))

(define (sail-bb vp frame)
  (let ((p (list (make-posn (/ viewport-size 2) 0)
                 (make-posn (/ viewport-size 2) viewport-size)
                 (make-posn viewport-size viewport-size))))
    (if (list? vp) 
        (for-each (lambda (port count) 
                    ((draw-solid-polygon port) (map (transform-posn frame) p)
                                               (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                  vp (build-list (length vp) values))
        ((draw-solid-polygon vp) (map (transform-posn frame) p)
                                 (make-posn 0 0)
                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))))

(define (corner-bb vp frame)
  (let ((p (list (make-posn (/ viewport-size 2) 0)
                 (make-posn viewport-size 0)
                 (make-posn viewport-size 
                            (/ viewport-size 2)))))
    (if (list? vp) 
        (for-each (lambda (port count)
                    ((draw-solid-polygon port) (map (transform-posn frame) p)
                                               (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                  vp (build-list (length vp) values))
        ((draw-solid-polygon vp) (map (transform-posn frame) p) 
                                 (make-posn 0 0)
                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))))

(define (nova-bb vp frame)
  (let ((p (list (make-posn (/ viewport-size 2) 0)
                 (make-posn (/ viewport-size 4) (/ viewport-size 2))
                 (make-posn viewport-size (/ viewport-size 2))
                 (make-posn (/ viewport-size 2) (/ viewport-size 4)))))
    (if (list? vp)
        (for-each (lambda (port count)
                    ((draw-solid-polygon port) (map (transform-posn frame) p)
                                               (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                  vp (build-list (length vp) values))
        ((draw-solid-polygon vp) (map (transform-posn frame) p)
                                 (make-posn 0 0)
                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))))

(define (heart-bb vp frame)
  (let* ((k (/ (sqrt 2) 2))
         (1-k/1+3k (/ (- 1 k) (+ 1 (* 3 k))))
         (1-k/1+k (/ (- 1 k) (+ 1 k)))
         (1+k/1+3k (/ (+ 1 k) (+ 1 (* 3 k))))
         (p (list (make-posn (/ viewport-size 2) (* 1-k/1+3k viewport-size))
                  (make-posn (* 1-k/1+k (/ viewport-size 2))
                             (* 1+k/1+3k viewport-size))
                  (make-posn (/ viewport-size 2) viewport-size)
                  (make-posn (- viewport-size (* 1-k/1+k (/ viewport-size 2)))
                             (* 1+k/1+3k viewport-size)))))
    ; Draw a kite (bottom half of the heart).
    (if (list? vp)
        (for-each (lambda (port count)
                    ((draw-solid-polygon port) (map (transform-posn frame) p)
                                               (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                  vp (build-list (length vp) values))
        ((draw-solid-polygon vp) (map (transform-posn frame) p) 
                                 (make-posn 0 0)
                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))

    ; Draw the top of the heart.
    (let ((heart-circle
           (stack-frac (/ 2 (+ 1 (* 3 k)))
                       (quarter-turn-right (stack-frac (/ k (+ 1 k)) blank-bb circle-bb))
                       blank-bb)))
      (heart-circle vp frame)
      ((flip-horiz heart-circle) vp frame))))

; center-and-fill will center and scale a 2x2 image to fill the entire viewport.
; This is used by circle-bb, spiral-bb, and ribbon-bb.
(define center (make-posn (/ viewport-size 2) (/ viewport-size 2)))
(define (center-and-fill x)
  (add-vect center (scale-vect (/ viewport-size 2) x)))
  
(define (circle-bb vp frame)
  ; make-circle will return a list of points (a lot of points) that approx a circle.
  (define (make-circle)
    (define (helper angle poly)
      (if (>= angle (* 2 pi))
          poly
          (cons (make-posn (cos angle) (sin angle))
                (helper (+ angle (/ 1 viewport-size)) poly))))
    (helper 0 ()))

  ; We approximate a circle by drawing polygon with A LOT of vertices.
  (if (list? vp)
      (for-each (lambda (port count)
                  ((draw-solid-polygon port) (map (transform-posn frame) (map center-and-fill (make-circle)))
                                             (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                             (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                vp (build-list (length vp) values))
      ((draw-solid-polygon vp) (map (transform-posn frame) (map center-and-fill (make-circle)))
                               (make-posn 0 0)
                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame)))))


(define (spiral-bb vp frame)
  (let* ((theta-max 30)
         (thickness (/ -1 theta-max)))
    
    (define (make-spiral)
      (define (helper angle offset poly)
        (if (>= angle theta-max)
            poly
            (cons (make-posn (* (+ offset (/ angle theta-max)) (cos angle))
                             (* (+ offset (/ angle theta-max)) (sin angle)))
                  (helper (+ angle 0.02) offset poly))))
      (helper 0 0 (reverse (helper 0 thickness '()))))
   
    (if (list? vp)
        (for-each (lambda (port count)
                    ((draw-solid-polygon port) (map (transform-posn frame) (map center-and-fill (make-spiral)))
                                               (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                  vp (build-list (length vp) values))
        ((draw-solid-polygon vp) (map (transform-posn frame) 
                                      (map center-and-fill (make-spiral)))
                                 (make-posn 0 0)
                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))))

(define (ribbon-bb vp frame)
  (let* ((theta-max 30)
         (thickness (/ -1 theta-max)))
    
    (define (make-ribbon angle poly)
      (if (>= angle theta-max)
          (close-ribbon theta-max poly)
          (cons (make-posn (* (/ angle theta-max) (cos angle))
                           (* (/ angle theta-max) (sin angle)))
                (make-ribbon (+ angle 0.02) poly))))
    
    (define (close-ribbon angle poly)
      (if (<= angle 0)
          poly
          (cons (make-posn (+ (abs (* (cos angle) thickness))
                              (* (/ angle theta-max) (cos angle)))
                           (+ (abs (* (sin angle) thickness))
                              (* (/ angle theta-max) (sin angle))))
                (close-ribbon (- angle 0.02) poly))))
    
    (if (list? vp)
        (for-each (lambda (port count)
                    ((draw-solid-polygon port) (map (transform-posn frame) (map center-and-fill (make-ribbon 0 ())))
                                               (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                               (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                  vp (build-list (length vp) values))
        ((draw-solid-polygon vp) (map (transform-posn frame)
                                      (map center-and-fill (make-ribbon 0 ())))
                                 (make-posn 0 0)
                                 (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))))

(define (black-bb vp frame)
 (let ((p (list (make-posn 0 0)
                (make-posn viewport-size 0)
                (make-posn viewport-size viewport-size)
                (make-posn 0 viewport-size))))
   (if (list? vp)
       (for-each (lambda (port count)
                   ((draw-solid-polygon port) (map (transform-posn frame) p)
                                              (make-posn (* (- 0.3 (frame-z1 frame)) (* spread (- (/ (* 2 count) (- (length vp) 1)) 1))) 0)
                                              (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))
                 vp (build-list (length vp) values))
       ((draw-solid-polygon vp) (map (transform-posn frame) p)
                                (make-posn 0 0)
                                (make-rgb (frame-z1 frame) (frame-z1 frame) (frame-z1 frame))))))

; (void) prevents PLT scheme from displaying anything.
; An alternative is to use (when #f ()).
(define (blank-bb vp frame) (void))
                           

; Custom painter maker

(define (image->painter filename)
  (let ((converter (open-pixmap "Converter" viewport-size viewport-size))
        (tolerance (/ 1 spread))
        (limit 0.86))
    
    (define (outer-loop y)
      (define (inner-loop x y)
        (if (< x viewport-size)
            (let* ((color ((get-color-pixel converter) (make-posn x y)))
                   (r (rgb-red color))
                   (g (rgb-green color))
                   (b (rgb-blue color))
                   (mono (/ (+ r g b) 3)))
              (cons mono (inner-loop (+ x 1) y)))
            ()))
      (if (< y viewport-size)
          (cons (inner-loop 0 y) (outer-loop (+ y 1)))
          ()))
    
    ((draw-pixmap converter) filename (make-posn 0 0))
    
    (let ((data (eval (cons 'vector (map (lambda (x) (eval (cons 'vector x))) (outer-loop 0))))))

      (define (get-depth x y dir) ; lp -> dir = -1, rp -> dir = 1
        (let ((result 1))
          (define (loop c)
            (let ((ox (round-off (+ x (* dir (+ (* -0.3 spread) c))))))
              (if (and (>= ox 0) (< ox viewport-size))
                  (let* ((curr (vector-ref (vector-ref data (inexact->exact (round-off y))) (inexact->exact (round-off ox))))
                         (d (abs (- curr (/ c spread)))))
                    (if (< d tolerance)
                        (set! result curr)
                        (if (< c spread)
                            (loop (+ c 1)))))
                  (if (< c spread)
                      (loop (+ c 1))))))
          (loop 0)
          result))
      
      (lambda (vp frame)
        
        (define (ana-out-loop y port count)
          (define (ana-in-loop x)
            (let* ((col (get-depth x y count))
                   (col (if (> col limit) 999 (+ (frame-z1 frame) (* (- (frame-z2 frame) (frame-z1 frame)) col)))))
              (if (<= col 1)
                  ((draw-pixel port) (apply (transform-posn frame) (list (make-posn x y)))
                                     (make-rgb col col col)))
              (if (< x (- viewport-size 1))
                  (ana-in-loop (+ x 1)))))
          (ana-in-loop 0)
          (if (< y (- viewport-size 1))
              (ana-out-loop (+ y 1) port count)))
        
        (define (out-loop y)
          (define (in-loop x)
            (let* ((color (vector-ref (vector-ref data (inexact->exact (round-off y))) (inexact->exact (round-off x))))
                   (color (if (> color limit) 999 (+ (frame-z1 frame) (* (- (frame-z2 frame) (frame-z1 frame)) color)))))
              (if (<= color 1)
                  ((draw-pixel vp) (apply (transform-posn frame) (list (make-posn x y)))
                                   (make-rgb color color color)))
              (if (< x (- viewport-size 1))
                  (in-loop (+ x 1)))
              ))
          (in-loop 0)
          (if (< y (- viewport-size 1))
              (out-loop (+ y 1))))
        
        (if (list? vp)
            (for-each (lambda (port count) (ana-out-loop 0 port (- (/ (* 2 count) (- (length vp) 1)) 1))) vp (build-list (length vp) values))
            (out-loop 0))))))


; This procedure creates a painter from a depth function f(x,y) where 0<=x,y<=600. The
; depth function returns a depth value, a real number in the range [0,1] where 0 represents
; shallowest depth and 1 deepest. The depth function will be sampled at integer intervals,
; users may use this fact to aid them in writing the depth function.
(define (function->painter depth-fun)
  (define tolerance (/ 1 spread))
  (define (integer-round-off f) (inexact->exact (round-off f)))
  
  (let* ((integer-sequence (build-list viewport-size values))
         (data (list->vector (map (lambda (y) (list->vector (map (lambda (x) (depth-fun x y)) integer-sequence)) )integer-sequence))))
    
    (define (get-depth x y dir frame) ; lp -> dir = -1, rp -> dir = 1
      (let ((result 1))
        (define (loop c)
          (let ((ox (round-off (+ x (* dir (+ (* -0.3 spread) c))))))
            (if (and (>= ox 0) (< ox viewport-size))
                (let* ((curr (vector-ref (vector-ref data (inexact->exact (round-off y))) (inexact->exact ox)))
                       (curr (if (= curr 1) 1 (+ (frame-z1 frame) (* (- (frame-z2 frame) (frame-z1 frame)) curr))))
                       (d (abs (- curr (/ c spread)))))
                  (if (< d tolerance)
                      (set! result curr)
                      (if (< c spread)
                          (loop (+ c 1)))))
                (if (< c spread)
                    (loop (+ c 1))))))
        (loop 0)
        result))
    
    (lambda (vp frame)

      (define (ana-out-loop y port count)
          (define (ana-in-loop x)
            (let* ((col (get-depth x y count frame)))
              (if (< col 1)
                  ((draw-pixel port) (apply (transform-posn frame) (list (make-posn x y)))
                                     (make-rgb col col col)))
              (if (< x (- viewport-size 1))
                  (ana-in-loop (+ x 1)))))
        (ana-in-loop 0)
        (if (< y (- viewport-size 1))
            (ana-out-loop (+ y 1) port count)))
        
        (define (out-loop y)
          (define (in-loop x)
            (let* ((color (vector-ref (vector-ref data (inexact->exact (round-off y))) (inexact->exact (round-off x))))
                   (color (if (= color 1) 1 (+ (frame-z1 frame) (* (- (frame-z2 frame) (frame-z1 frame)) color)))))
              (if (< color 1)
                  ((draw-pixel vp) (apply (transform-posn frame) (list (make-posn x y)))
                                   (make-rgb color color color)))
              (if (< x (- viewport-size 1))
                  (in-loop (+ x 1)))))
      
          (in-loop 0)
          (if (< y (- viewport-size 1))
              (out-loop (+ y 1))))
        
      (if (list? vp)
          (for-each (lambda (port count) (ana-out-loop 0 port (- (/ (* 2 count) (- (length vp) 1)) 1))) vp (build-list (length vp) values))
          (out-loop 0))
        )))

; Frame transformation factory.
(define (process-frame op frame)
  (let* ((p0 (frame-orig frame))
         (p1 (frame-x frame))
         (p2 (frame-y frame))
         (z1 (frame-z1 frame))
         (z2 (frame-z2 frame)))
    (cond ((eq? op 'bottom-frac)
           (lambda (frac)
             (make-frame (add-vect p0 (scale-vect (- 1 frac) p2))
                         p1
                         (scale-vect frac p2)
                         z1
                         z2)))
          ((eq? op 'top-frac)
           (lambda (frac)
             (make-frame p0
                         p1
                         (scale-vect frac p2)
                         z1
                         z2)))
          ((eq? op 'left)
           (make-frame p0
                       (scale-vect (/ 1 2) p1)
                       p2
                       z1
                       z2))
          ((eq? op 'right)
           (make-frame (add-vect p0 (scale-vect (/ 1 2) p1))
                       (scale-vect (/ 1 2) p1)
                       p2
                       z1
                       z2))
          ((eq? op 'flip-horiz)
           (make-frame (add-vect p0 p1)
                       (scale-vect -1 p1)
                       p2
                       z1
                       z2))
          ((eq? op 'flip-vert)
           (make-frame (add-vect p0 p2)
                       p1
                       (scale-vect -1 p2)
                       z1
                       z2))
          ((eq? op 'reduce2)
           (make-frame (add-vect p0 (add-vect (scale-vect -0.4 p1)
                                              (scale-vect 0.125 p2)))
                       (scale-vect 0.7071 p1)
                       (scale-vect 0.7071 p2)
                       z1
                       z2))
          ((eq? op 'rotate)
           (lambda (rad)
             (let ((cos-theta (cos rad))
                   (sin-theta (sin rad)))
               (define (rotate-posn p)
                 (make-posn (+ (* cos-theta (posn-x p))
                               (* sin-theta (posn-y p)))
                            (- (* cos-theta (posn-y p))
                               (* sin-theta (posn-x p)))))
               (let* ((half-gradient (scale-vect 1/2 (add-vect p1 p2)))
                      (center (add-vect (add-vect p0 half-gradient)
                                        (rotate-posn (scale-vect -1 half-gradient)))))
                 (make-frame center
                             (rotate-posn p1)
                             (rotate-posn p2)
                             z1
                             z2)))))
          ((eq? op 'rotate90)
           (make-frame (add-vect p0 p1)
                       p2
                       (scale-vect -1 p1)
                       z1
                       z2))
          ((eq? op 'deep-frac)
           (lambda (frac)
             (make-frame p0
                         p1
                         p2
                         (+ z1 (* (- z2 z1) frac))
                         z2)))
          ((eq? op 'shallow-frac)
           (lambda (frac)
             (make-frame p0
                         p1
                         p2
                         z1
                         (+ z1 (* (- z2 z1) frac)))))
          ((eq? op 'scale-independent)
           (lambda (ratio-x ratio-y)
             (let* ((gradient (add-vect p1 p2))
                    (scaled-gradient (make-posn (* (/ (- 1 ratio-x) 2) (posn-x gradient))
                                                (* (/ (- 1 ratio-y) 2) (posn-y gradient))))
                    (center (add-vect p0 scaled-gradient)))
               (make-frame center
                           (scale-vect ratio-x p1)
                           (scale-vect ratio-y p2)
                           z1
                           z2))))
          ((eq? op 'translate)
           (lambda (x y)
             (make-frame (add-vect (add-vect p0 (scale-vect x p1)) (scale-vect y p2))
                         p1
                         p2
                         z1
                         z2))))))

; Basic painter combinators

(define (stack painter1 painter2)
  (stack-frac 1/2 painter1 painter2))

(define (stack-frac frac painter1 painter2)
  (lambda (vp frame)
    (let* ((uf ((process-frame 'top-frac frame) frac))
           (lf ((process-frame 'bottom-frac frame) (- 1 frac))))
      (painter1 vp uf)
      (painter2 vp lf))))

(define (rotate rad painter)
  (lambda (vp frame)
    (painter vp ((process-frame 'rotate frame) rad))))

(define (eighth-turn-left painter)
  (rotate (/ pi 4) painter))

(define (quarter-turn-right painter)
  (lambda (vp frame)
    (painter vp (process-frame 'rotate90 frame))))

(define (flip-horiz painter)
  (lambda (vp frame)
    (painter vp (process-frame 'flip-horiz frame))))

(define (flip-vert painter)
  (lambda (vp frame)
    (painter vp (process-frame 'flip-vert frame))))

(define (overlay painter1 painter2)
  (overlay-frac 1/2 painter1 painter2))

(define (overlay-frac frac painter1 painter2)
  (lambda (vp frame)
    (if (or (< 1 frac) (< frac 0))
        (error "overlay-frac: invalid fraction")
        (let* ((df ((process-frame 'deep-frac frame) frac))
               (sf ((process-frame 'shallow-frac frame) frac)))
          (painter2 vp df)
          (painter1 vp sf)))))

(define (scale-independent ratio-x ratio-y painter)
  (lambda (vp frame)
    (painter vp ((process-frame 'scale-independent frame) ratio-x ratio-y))))

(define (scale ratio painter)
  (scale-independent ratio ratio painter))

; Translate the painter. Note that positive x means translate right,
; positive y means translate down.
(define (translate x y painter)
  (lambda (vp frame)
    (painter vp ((process-frame 'translate frame) x y))))


; Painter combinations defined in lecture 2
(define (turn-upside-down painter)
  (quarter-turn-right (quarter-turn-right painter)))

(define (quarter-turn-left painter)
  (turn-upside-down (quarter-turn-right painter)))

(define (beside painter1 painter2) 
  (quarter-turn-right 
   (stack (quarter-turn-left painter2) 
          (quarter-turn-left painter1))))

(define (make-cross painter)
  (stack (beside (quarter-turn-right painter)
                 (turn-upside-down painter))
         (beside painter
                 (quarter-turn-left painter))))

(define (repeat-pattern n pat pic)
  (if (= n 0)
      pic
      (pat (repeat-pattern (- n 1) pat pic))))

(define (stackn n painter)
  (if (= n 1)
      painter
      (stack-frac (/ 1 n) painter (stackn (- n 1) painter))))

; Red-Blue  generation function
(define (anaglyph painter)
  (let* ((viewport-size 600)
         (unit-frame (make-frame (make-posn (* 1/6 viewport-size) 0)
                                 (make-posn viewport-size 0)
                                 (make-posn 0 viewport-size)
                                 0
                                 1))
         (MAX_X (round-off (* 4/3 viewport-size)))
         (MAX_Y viewport-size)
         (stereo vp))
    
    (define (get-depth x y vp)
      (if (and (>= x 0) (< x MAX_X))
          (rgb-red ((get-color-pixel vp) (make-posn x y)))
          1))
    
    (define (row-loop y) ; Generate anaglyph row by row
      (define (paint x) ; paint the anaglyph viewport
        (let* ((l (get-depth x y lp))
               (r (get-depth x y rp))
               (colour (make-rgb r l l)))
          ((draw-pixel stereo) (make-posn x y) colour)
          (if (< (+ x 1) MAX_X)
              (paint (+ x 1)))))
            
        (paint 0)
        
        (if (< (+ y 1) MAX_Y)
            (row-loop (+ y 1))))
    
    (painter (list vp vp) unit-frame)
    (painter (list lp rp) unit-frame)
    (row-loop 0)
    (display "done")
    (newline)
    ))

; Stereogram generation function
(define (stereogram painter)
  (let* ((viewport-size 600)
         (unit-frame (make-frame (make-posn 0 0)
                                 (make-posn viewport-size 0)
                                 (make-posn 0 viewport-size)
                                 0
                                 1))
         (E 300) ; distance between eyes, 300 pixels
         (D 600) ; distance between eyes and image plane, 600 pixels
         (DELTA 40) ; stereo seperation
         (MAX_X (inexact->exact (round-off (* 4/3 viewport-size)))) ; stereogram viewport to be 3 times as wide to 
         (MAX_Y viewport-size)
         (MAX_Z 0)
         (CENTRE (round-off (/ MAX_X 2)))
         (stereo vp)
         (depth (open-pixmap "Depthmap Viewport" viewport-size viewport-size)))
    
    (define (get-depth x y) ; translates greyscale depthmap into numerical depth data.
                            ; white = -500 (furthest), black = -400 (nearest)
      (if (and (>= x (* 1/6 viewport-size)) (< x (- MAX_X (* 1/6 viewport-size))))
          (+ -400 (* -100 (rgb-red ((get-color-pixel depth) (make-posn (- x (* 1/6 viewport-size)) y)))))
          -500))
    
    (define (row-loop y) ; Generate stereogram row by row
      (let ((link-left (make-vector MAX_X))
            (link-right (make-vector MAX_X))
            (colours (make-vector MAX_X)))

        (define (column-loop x) ; Runs through a row
          (let* ((z (get-depth x y)) ; Obtain depth
                 (s (+ (* z (/ E (- z D))) DELTA)) ; Determine distance between intersection
                                                   ; of lines of sight on image plane
                 (left (inexact->exact (- x (round-off (/ s 2))))) ; Determine coordinates of intersection of
                                              ; line of sight from left eye
                 (right (inexact->exact (+ left (round-off s))))) ; Determine coordinates of intersection of
                                             ; line of sight from right eye
            (if (and (> left 0) (< right MAX_X)) ; Proceed if within bounds
                (if (and (or (not (vector-ref link-right left)) ; check if new contraint needs to be updated
                             (< s (vector-ref link-right left))) ; update only if no constraint exist or
                         (or (not (vector-ref link-left right)) ; if new constraint is of a smaller separation
                             (< s (vector-ref link-left right))))
                    (begin
                      (vector-set! link-right left (round-off s)) ; update right linkage
                      (vector-set! link-left right (round-off s))))) ; update left linkage
            (if (< (+ x 1) MAX_X) 
                (column-loop (+ x 1)))))
        
        (define (confirm x) ; compare right and left linkage vectors to eliminate overwritten constraints
          (let* ((s (vector-ref link-left x))
                 (s (if (not s)
                        +inf.0
                        x))
                 (d (if (< 0 (- x s))
                        (vector-ref link-right (- x s))
                        +inf.0)))
            (if (or (= s +inf.0) (> s d))
                (vector-set! link-left x 0))
            (if (< (+ x 1) MAX_X)
                (confirm (+ x 1)))))
        
        (define (paint x) ; paint the stereogram viewport
          (let* ((s (vector-ref link-left x))
                 (colour (vector-ref colours (inexact->exact (- x s))))
                 (colour (if (not colour)
                             (make-rgb (/ (random 10) 9) (/ (random 10) 9) (/ (random 10) 9))
                             colour)))
;            (if (not (= s 0))
; if constraint linkage exists copy pixel, else create new random pixel
;                ((draw-pixel stereo) (make-posn x y)
; ((get-color-pixel stereo) (make-posn (- x s) y)))
;                ((draw-pixel stereo) (make-posn x y)
; (make-rgb (/ (random 10) 9) (/ (random 10) 9) (/ (random 10) 9)))) ; 1000 colours
            ((draw-pixel stereo) (make-posn x y) colour)
            (vector-set! colours x colour)
            (if (< (+ x 1) MAX_X)
                (paint (+ x 1)))))
        
        (vector-fill! link-left #f)
        (vector-fill! link-right #f)
        (vector-fill! colours #f)
      
        (column-loop 0)
        (confirm 0)
        (paint 0)
        
        (if (< (+ y 1) MAX_Y)
            (row-loop (+ y 1)))))
    
    (painter depth unit-frame)
    (row-loop 0)
    (display "done")
    (newline)
;    ((save-pixmap stereo)
;     (string-append "Stereogram" 
;                    (number->string (remainder (current-seconds) 10000))
;                    ".bmp") 'bmp) ; save stereogram
    (close-viewport depth)))

; Save File

(define (save-as-bmp name)
  ((save-pixmap vp) name 'bmp))

(define (save-as-xpm name)
  ((save-pixmap vp) name 'xpm))

(define (save-as-gif name)
  (if (not (null? active-hollusion))
      (let ((y-list (build-list viewport-size values))
            (x-list (build-list (inexact->exact (round-off (* 4/3 viewport-size))) values)))
        
        (define (flatten tree)
          (cond ((null? tree) null)
                ((list? (car tree)) (flatten (append (car tree) (cdr tree))))
                (else (cons (car tree) (flatten (cdr tree))))))
        
        (define (get-bytes vp)
          (apply bytes (flatten (map (lambda (y) 
                                       (map (lambda (x)
                                              (inexact->exact (round-off (* 255 (rgb-red ((get-color-pixel vp) (make-posn x y))))))) 
                                            x-list)) 
                                     y-list))))
        
        (let* ((buffer-bytes (map get-bytes (active-hollusion 'buffers)))
               (buffer-bytes (append (cdr buffer-bytes) (cdr (reverse buffer-bytes))))
               (interval (inexact->exact (round-off (/ 50 (length buffer-bytes)))))
               (interval (if (< interval 1) 1 interval))
               (cmap (build-list 256 (lambda (x) (list->vector (list x x x)))))
               (file (open-output-file name))
               (the-gif (gif-start file (* 4/3 viewport-size) viewport-size 255 cmap)))
          (gif-add-loop-control the-gif 0)
          (for-each (lambda (port-bytes)
                      (gif-add-control the-gif 'any #f interval  #f)
                      (gif-add-image the-gif 0 0 (* 4/3 viewport-size) viewport-size #f #f port-bytes))
                    buffer-bytes)
          (gif-end the-gif)
          (close-output-port file)))
;        ((save-pixmap lp) (string-append "lp-" name) 'bmp)
;        ((save-pixmap (active-hollusion 'buffer)) (string-append "mp-" name) 'bmp)
;        ((save-pixmap rp) (string-append "rp-" name) 'bmp))
      (error "unable to save: no active animation")))

(define (save-custom-animation name vps intervals)
  (let ((y-list (build-list viewport-size values))
        (x-list (build-list (inexact->exact (round-off (* 4/3 viewport-size))) values)))
    
    (define (flatten tree)
      (cond ((null? tree) null)
            ((list? (car tree)) (flatten (append (car tree) (cdr tree))))
            (else (cons (car tree) (flatten (cdr tree))))))
    
    (define (get-bytes vp)
      (display ".")
      (apply bytes (flatten (map (lambda (y) 
                                   (map (lambda (x)
                                          (inexact->exact (round-off (* 255 (rgb-red ((get-color-pixel vp) (make-posn x y))))))) 
                                        x-list)) 
                                 y-list))))
    
    (display "rendering...")
    (let* ((buffer-bytes (map get-bytes vps))
           (cmap (build-list 256 (lambda (x) (list->vector (list x x x)))))
           (file (open-output-file name))
           (the-gif (gif-start file (* 4/3 viewport-size) viewport-size 255 cmap)))
      (newline)
      (display "saving...")
      (gif-add-loop-control the-gif 0)
      (for-each (lambda (port-bytes interval)
                  (gif-add-control the-gif 'any #f interval  #f)
                  (gif-add-image the-gif 0 0 (* 4/3 viewport-size) viewport-size #f #f port-bytes))
                buffer-bytes intervals)
      (gif-end the-gif)
      (close-output-port file)
      (newline)
      (display "done"))))

; Some examples

;(stereogram (overlay (scale 0.8 heart-bb) circle-bb))
; (show (overlay-frac 0
;                        (overlay-frac 0 spiral-bb (quarter-turn-right spiral-bb))
;                        (quarter-turn-right
;                        (quarter-turn-right(overlay-frac 0 spiral-bb 
;                                                          (quarter-turn-right spiral-bb))))))


; (define pic (image->painter "cs1101s.jpg"))
; (define pic2 (overlay pic heart-bb))
; (show pic2)
; (stereogram pic2)

(define (square x) (* x x))

; Sample depth map.
; Note that radius1 should be < than radius2. Can you see why?
; How would you modify this such that the requirement is no longer necessary?
(define (create-conc-circle-zf radius1 depth1 radius2 depth2)
  (let ((a1^2 (square radius1))
        (a2^2 (square radius2)))
    (lambda (x y)
      (define (helper x y)
        (let ((x^2+y^2 (+ (square x) (square y))))
          (cond ((< x^2+y^2 a1^2) depth1)
                ((< x^2+y^2 a2^2) depth2)
                (else 1))))
      (helper (- x 300) (- y 300)))))
 
(define (hollusion painter . ports)
  (let* ((frequency 2)
         (viewport-size 600)
         (unit-frame (make-frame (make-posn (* 1/6 viewport-size) 0)
                                 (make-posn viewport-size 0)
                                 (make-posn 0 viewport-size)
                                 0
                                 1))
         (MAX_X (round-off (* 4/3 viewport-size)))
         (MAX_Y viewport-size)
         (num (if (null? ports) 3 (if (< 2 (car ports)) (car ports) 3)))
         (buffers (build-list num (lambda (x) (open-pixmap "buffer" (* (/ 4 3) viewport-size) viewport-size))))
         (stereo vp))
    
    (define (animation)
      (let ((ports (append (cdr buffers) (cdr (reverse buffers))))
            (kill #f)
            (self null))
        (set! self (lambda (message)
                     (case message
                       ((next) (if (not kill)
                                   (begin
                                     (set! ports (append (cdr ports) (list (car ports))))
                                     (copy-viewport (car ports) stereo)
                                     (sleep (/ 1 (* frequency (length ports))))
                                     (self 'next))))
                       ((kill) (begin
                                 (set! kill #t)
                                 (for-each close-viewport buffers)))
                       ((buffers) buffers))))
        self))
    
    (painter buffers unit-frame)
    
    (if (not (null? active-hollusion))
        (begin
          (active-hollusion 'kill)
          (kill-thread hollusion-thread)))
    
    (set! active-hollusion (animation))
    (set! hollusion-thread (thread (lambda () (active-hollusion 'next))))
    (display "done")
    (newline)
    ))				