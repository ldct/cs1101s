(load "runes.rkt")

;; anaglyph!

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
  (overlay (beside-frac (/ 1 n)
                        stacked
                        (beside-frac (/ (- n 2) (- n 1))
                                     blank-bb
                                     stacked))
           (beside-frac (/ 1 n)
                        blank-bb
                        (beside-frac (/ (- n 2) (- n 1))
                                     p
                                     blank-bb))))

(define (persian rune count)
 (define stacked (beside-n count rune))
 (define n count)
 (stack-frac (/ 1 n)
                       stacked
                       (stack-frac (/ (- n 2) (- n 1))
                                   (left-guard n rune)
                                   stacked)))
           

(define (fp)
  (persian (persian (persian (make-cross rcross-bb) 4) 4) 4))

; Test
(anaglyph (fp))
;(stereogram (persian (make-cross rcross-bb) 4))