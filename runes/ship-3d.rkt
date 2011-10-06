(load "runes.rkt")

;; anaglyph
;; warning; takes very long to render

(define (square z) (* z z))
(define (ship-square z)
  (square (+ (abs (real-part z))
             (* 0+i (abs (imag-part z))))))
  
(define (gray x) (overlay-frac x blank-bb black-bb))

(define n 19)

(define (mandelbrot? c)
  (define (iter count z)
    (if (or (= count 0) (< 2 (magnitude z)))
        count
        (iter (- count 1) (+ (ship-square z) c))))
    (iter n 0))

(define (map-from-screen x y)
  (make-rectangular (/ x 5200.) (/ y 5200.)))

(define (mandelbrot-colour x y)
  (gray (/ (mandelbrot? (map-from-screen x y)) n)))

(define step 1)

(define (beside-frac frac a b)
  (quarter-turn-left (stack-frac frac 
                                 (quarter-turn-right a)
                                 (quarter-turn-right b))))

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

(anaglyph (render -9350 -8750 -450 150))