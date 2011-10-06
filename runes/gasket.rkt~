;
; CS1101S --- Programming Methodology
;
; Mission 2
;
; Name: Li Xuanji
; Matric no: A0091997H
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "runes.rkt")


;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (mosaic a b c d)
 (stack (beside d a)
        (beside c b)))

; Test
; (show (mosaic rcross-bb sail-bb corner-bb nova-bb))


;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (simple-fractal p)
 (beside p (stack p p)))

; Test
(show (simple-fractal (make-cross rcross-bb)))


;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;

(define (fractal p n)
  (if (= n 2)
      (simple-fractal p)
      (beside p (stack (fractal p (- n 1)) 
                       (fractal p (- n 1))))))

; Test
;(show (fractal (make-cross rcross-bb) 3))