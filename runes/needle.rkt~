(load "runes.rkt")

(define (square z) (* z z))

(define (gray x) (overlay-frac x blank-bb black-bb))
(define (beside-frac frac a b)
  (quarter-turn-left (stack-frac frac 
                                 (quarter-turn-right a)
                                 (quarter-turn-right b))))

(define n 23)

(define (mandelbrot c)
  (define (iter count z)
    (if (or (< 2 (magnitude z)) (= count 0))
        count
        (iter (- count 1) (+ (square z) c))))
    (iter n 0))

(define (map-from-screen x y)
  (make-rectangular (/ x 270.) (/ y 270.)))

(define (mandelbrot-colour x y)
  (gray (/ (mandelbrot (map-from-screen x y)) n)))

(define step 1)
                   
(define (render x1 x2 y1 y2)
  (define dx (- x2 x1))
  (define dy (- y2 y1))
  (if (= 1 dx)
      (if (= 1 dy)
          (mandelbrot-colour x1 y1)
          (if (even? dy)
              (stack (render x1 x2 y1 (+ (/ dy 2) y1))
                     (render x1 x2 (+ (/ dy 2) y1) y2))
              (stack-frac (/ 1 dy)
                          (render x1 x2 y1 (+ 1 y1))
                          (render x1 x2 (+ 1 y1) y2))))
      (if (even? dx)
          (beside (render x1 (+ (/ dx 2) x1) y1 y2)
                  (render (+ (/ dx 2) x1) x2 y1 y2))
          (beside-frac (/ 1 dx)
                       (render x1 (+ 1 x1) y1 y2)
                       (render (+ 1 x1) x2 y1 y2)))))

(show (render -550 50 -450 150))