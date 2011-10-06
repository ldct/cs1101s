;
; CS1101S --- Programming Methodology
;
; Mission 6 - Side Quest "Kochize"
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "hi-graph-sidequests.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define pi3 (rotate-around-origin (/ pi 3)))
(define -pi3 (rotate-around-origin (- (/ pi 3))))

(define (koch n curve)
  (if (= n 0)
      curve
      (let ((c (koch (- n 1) curve)))
        (put-in-standard-position
         (connect-ends (connect-ends c
                                     (pi3 c))
                       (connect-ends (-pi3 c)
                                     c))))))
                    
(define (show-connected-koch window level number-of-points)
  ((draw-connected window number-of-points) (koch level unit-line)))

; Test
;(show-connected-koch g2 5 4000)

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define k5 (koch 5 unit-line))

(define snowflake
  (connect-ends (connect-ends k5
                              ((rotate-around-origin (* pi -2/3)) k5))
                ((rotate-around-origin (* pi 2/3)) k5)))
  

; Test
;((draw-connected-full-view-proportional g3 10000) snowflake)


