;
; CS1101S --- Programming Methodology
;
; Mission 5
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "hi-graph.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (reflect-through-y-axis curve)
  (lambda (t)
    (define ct (curve t))
    (make-point (- (x-of ct))
                (y-of ct))))
  
((draw-connected-squeezed-to-window g1 10) alternative-unit-circle)
((draw-connected-squeezed-to-window g2 10) (reflect-through-y-axis alternative-unit-circle))

  

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

; recommended to use connect-rigidly

(define (connect-ends curve1 curve2)
  (define x1 (x-of (curve1 1)))
  (define y1 (y-of (curve1 1)))
  (define x2 (x-of (curve2 0)))
  (define y2 (y-of (curve2 0)))
  (connect-rigidly curve1 ((translate (- x1 x2) (- y1 y2)) curve2)))
  
;(define rline (vertical-line (make-point 0.5 0.25) 0.5))
;(define qline (vertical-line (make-point 0.25 0.25) 0.25))
;((draw-connected-squeezed-to-window g1 20) (connect-ends alternative-unit-circle rline))