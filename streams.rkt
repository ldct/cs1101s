;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CS1101S - PS07 support code
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; some basic stream operations

(require (lib "defmacro.ss"))
(require (lib "trace.ss"))

; Stream implementation for Dr.Scheme
(define-macro cons-stream 
  (lambda (x y) 
    `(cons ,x (delay ,y))))

(define the-empty-stream '()) 
(define (stream-car stream) (car stream)) 
(define (stream-cdr stream) (force (cdr stream))) 
(define (stream-null? stream) (null? stream)) 

; stream usage tools
(define (eval-stream s n)
  (if (or (= n 0) (stream-null? s))
      ()
      (cons (stream-car s) 
            (eval-stream (stream-cdr s) (- n 1)))))

(define (stream-take s n)
  (if (= n 0)
      the-empty-stream
      (cons-stream 
       (stream-car s) 
       (stream-take 
        (stream-cdr s) (- n 1)))))

(define (stream-tail s n)
  (if (= n 0)
      s
      (stream-tail (stream-cdr s) (- n 1))))

(define (show-stream s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-car s))
             (show-stream (stream-cdr s) (- nterms 1)))))

(define (write-line x) (display x) (newline))

(define (stream-ref s n) (stream-car (stream-tail s n)))





; stream building tools
(define (stream-constant k)
  (cons-stream k (stream-constant k)))

(define integers 
  (cons-stream 1 (add-streams (stream-constant 1) integers)))

(define (stream-map proc . argstreams) ; variable # of args
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream k s)
  (stream-map * (stream-constant k) s))

(define (add-streams . args)
  (apply stream-map (cons + args)))

(define (mul-streams . args)
  (apply stream-map (cons * args)))


(define (stream-pairs s)
  (if (stream-null? s)
      the-empty-stream
      (stream-append
       (stream-map
        (lambda (sn) 
          (list (stream-car s) sn))
        (stream-cdr s))
       (delay (stream-pairs (stream-cdr s))))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))



(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter pred 
                         (stream-cdr stream))))
        (else
         (stream-filter pred
                        (stream-cdr stream)))))

(define (replace str a b)
  (cons-stream 
   (if (equal? (stream-car str) a)
       b
       (stream-car str))
   (replace (stream-cdr str) a b)))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series -1 s))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))




;;; create a (finite) series from a list of coefficients
(define (coeffs->series list-of-coeffs)
  (define zeros (cons-stream 0 zeros))
  (define (iter list)
    (if (null? list)
        zeros
        (cons-stream (car list)
                     (iter (cdr list)))))
  (iter list-of-coeffs))

;;; convert a list to a finite stream
(define (list->stream l)
  (foldr (lambda (x y) (cons-stream x y)) '() l))

;;; create a series from a procedure: nth term is P(n)
;;; requires non-neg-integers to be 0,1,2,3....
(define (proc->series proc)
  (stream-map proc non-neg-integers))



