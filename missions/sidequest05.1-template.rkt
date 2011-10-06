; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "hi-graph.rkt")

(define test-curve
    (lambda (t)
      (make-point t (+ 0.5 (/ (sin (* 4 (* pi t))) 2)))))

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (scale-y frac curve)
  (lambda (t)
    (define ct (curve t))
    (make-point (x-of ct)
                (* (y-of ct) frac))))

(define (stack c1 c2)
  (connect-rigidly ((translate 0 (/ 2)) (scale-y (/ 2) c1))
                   (scale-y (/ 2) c2)))

; Test
;((draw-points-on g3 4000) (stack test-curve test-curve))

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (stack-frac frac c1 c2)
  (connect-rigidly ((translate 0 (- 1 frac)) (scale-y frac c1))
                   (scale-y (- 1 frac) c2)))

;((draw-points-on g3 4000)
; (stack-frac (/ 5) test-curve test-curve))


; Test Case

((draw-points-on g3 6000) 
   (stack-frac (/ 1 5) 
               test-curve 
               (stack-frac (/ 3 4) 
                           test-curve
                           test-curve)))
