;
; CS1101S --- Programming Methodology (Scheme)
; 
; Contest 2: Alien Pancakes
;
; Note that answers are commented out to allow the Tutors to 
; run your code easily while grading your problem set.

;===========
(load "force-crystal.rkt")

(define (ln2 x)
  (inexact->exact (/ (log x) (log 2))))

(define (bf-guesses lst)
  (foldr + 0 (map ln2 lst)))

(define (midpoint lower upper)
  (+ 1 (quotient (- upper lower) 2)))

(define (list-0 n)
  (if (= 0 n)
      '()
      (append '(0) (list-0 (- n 1)))))

(define (check-crystal pos level)
  (force-aura (append (
  

(define (bf-single pos lower upper)
  (if (= 1 (- upper lower))
      lower
      (if (check-crystal pos (midpoint lower upper))
          (bf-single pos (midpoint upper lower) upper)
          (bf-single pos lower (midpoint upper lower)))))

(define (force-crystal number-of-crystals number-of-params force-quantity-upperbound)
  (if (< number-crystals (bf-guesses force-quantity-upperbound))
      'hello
      'bye))