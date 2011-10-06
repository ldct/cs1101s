;
; CS1101S --- Programming Methodology
;
; Mission 4 - Side Quest "Defend Yourself"
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "hi-graph.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define lo 0.3)
(define hi 0.6)
(define ce 0.3)

(define (range t)
  (cond ((< t lo) 1)
        ((<= t hi) 2)
        (else 3)))

(define (J t)
  (cond ((= (range t) 1)
         (define theta (- (* (/ t lo) pi) pi))
         (make-point (+ (/ (cos theta) 4) ce) (+ (/ (sin theta) 4) ce)))
        ((= (range t) 2)
         (define u (* (- t lo) 2))
         (make-point (+ .25 ce) (+ u ce)))
        (else
         (define v (- t hi))
         (make-point (+ (* v 2) 0.1) 0.9))))
; Test
((draw-connected g3 10000) J)