;
; CS1101S --- Programming Methodology
;
; Mission 2 - Side Quest
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
(define (beside-frac frac a b)
  (quarter-turn-left (stack-frac frac 
                                 (quarter-turn-right a)
                                 (quarter-turn-right b))))

(define (stack-n n p)
  (if (= n 1)
      p
      (stack-frac (/ 1 n)
                  p
                  (stack-n (- n 1) p))))

(define (beside-n n p)
  (quarter-turn-left (stack-n n (quarter-turn-right p))))

(define (left-guard n p)
  (define stacked (stack-n (- n 2) p))
  (beside-frac (/ 1 n)
               stacked
               (beside-frac (/ (- n 2) (- n 1))
                            p
                            stacked)))

(define (persian rune count)
 (define stacked (beside-n count rune))
 (define n count)
 (stack-frac (/ 1 n)
             stacked
             (stack-frac (/ (- n 2) (- n 1))
                         (left-guard n rune)
                         stacked)))

; Test
(show (persian (make-cross rcross-bb) 5))