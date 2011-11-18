;
; CS1101S --- Programming Methodology
;
; Mission 20
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "streams.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (make-step-stream m)
  (cons-stream 1 (stream-map (lambda (n) (+ 1 (modulo n m)))
                             integers)))

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (make-oscillating-stream n)
  (if (= n 1)
      (stream-map (lambda (n) 1) integers)
      (stream-map (lambda (m) 
                    (define k (modulo m (- (* 2 n) 2)))
                    (cond ((= k 0) 2)
                          ((< k n) k)
                          (else (- (* 2 n) k))))
                  integers)))

;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;


(define (make-flexible-step-stream base)
  (cons-stream (car base)
               (make-flexible-step-stream (cdr (append base (list (car base)))))))

(define (make-flexible-oscillating-stream base)
  (make-flexible-step-stream (append (reverse (cdr (reverse base)))
                                     (reverse (cdr base)))))

;;;;;;;;;;
; Task 4 ;
;;;;;;;;;;

(define (interleave a b)
  (if (null? a)
      b
      (cons-stream (stream-car a)
                   (interleave b (stream-cdr a)))))

;;;;;;;;;;
; Task 5 ;
;;;;;;;;;;

(define (flexible-interleave s . rest)
  (if (null? s)
      (apply flexible-interleave rest)
      (cons-stream (stream-car s)
                   (apply flexible-interleave (append rest (list (stream-cdr s)))))))

;;;;;;;;;;
; Task 6 ;
;;;;;;;;;;

; Submit your answer for generating the Golomb Sequence

(define (count x lst)
  (length (filter (lambda (t) (= x t)) lst)))

(define (last lst)
  (car (reverse lst)))

(define (tagged-golomb partial)
  (define n (last partial))
  (define next
    (if (= (list-ref partial (- n 1)) (count n partial))
        (append partial (list (+ n 1)))
        (append partial (list n))))
  (cons-stream next (tagged-golomb next)))

(define golomb-stream (cons-stream 1 (stream-map last (tagged-golomb '(1)))))

(eval-stream golomb-stream 30)
