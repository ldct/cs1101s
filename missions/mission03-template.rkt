;
; CS1101S --- Programming Methodology
;
; Mission 3
;
; Name: Li Xuanji
; Matric no: A0091997H
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "runes.rkt")

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (inv n) (/ 1 n))

(define (tree n p)
  (define (treep i n p)
    (if (= i n) 
        p
        (overlay-frac (inv (+ 1 (- n i)))
                      (scale (/ i n) p)
                      (treep (+ 1 i) n p))))
  (treep 1 n p))

; Test
(stereogram (tree 16 circle-bb))