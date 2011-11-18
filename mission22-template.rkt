;
; CS1101S --- Programming Methodology
;
; Mission 22
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.

(load "streams.rkt")

(define (make-cosine-series)
  (define (helper sign val fact)
    (let* ((car-val (if (even? val)
                       (sign (/ 1 fact))
                       0))
          (new-sign (if (= car-val 0)
                        sign
                        (if (eq? sign +) - +)))
          (new-val (+ val 1))
          (new-fact (* fact new-val)))
    (cons-stream 
     car-val
     (helper new-sign new-val new-fact))))
  
  (cons-stream 1
               (helper - 1 1)))

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (fold-stream op stream init)
  (cons-stream init (fold-stream op (stream-cdr stream) 
                                 (op init (stream-car stream)))))

(define (gp-stream x0)
  (fold-stream * (stream-constant x0) 1))

(define (approximate x0 series)
  (stream-cdr (fold-stream + 
                           (mul-streams series (gp-stream x0))
                           0)))

; sample test
(eval-stream (approximate pi (make-cosine-series)) 5)
; (1 1 -3.934802200544679 -3.934802200544679 0.12390992587208849)

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (sum-first n stream)
  (if (= n 0)
      0
      (+ (stream-car stream)
         (sum-first (- n 1) (stream-cdr stream)))))

(define (sum-triangle n sos)
  (if (= n 0)
      0
      (+ (sum-first n (stream-car sos))
         (sum-triangle (- n 1) (stream-cdr sos)))))

(define (f-cdr f)
  (lambda (x)
    (f (+ x 1))))

(define (fun->stream f)
  (cons-stream (f 0)
               (fun->stream (f-cdr f))))

(define (greater-approximate x0 stream-of-series)
  (stream-cdr 
   (fun->stream 
    (lambda (n)
      (sum-triangle n (stream-map (lambda (x)
                                    (mul-streams (gp-stream x0)
                                                 x))
                                  stream-of-series))))))

; sample test
(define approx-cosine-series 
  (cons-stream (scale-stream 0.5 (make-cosine-series)) 
               (stream-map (lambda (x) (scale-stream 0.5 x)) approx-cosine-series)))

(eval-stream (greater-approximate pi approx-cosine-series) 5)
; (0.5 0.75 -1.5924011002723395 -2.7636016504085092 -1.3198458622682103)

;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;

(define (interleave a b)
  (cons-stream (stream-car a)
               (interleave b (stream-cdr a))))

(define (zip-over x s)
  (stream-map (lambda (t) (list x t))
              s))

(define (interleave a b)
  (if (stream-null? a)
      b
      (cons-stream (stream-car a)
                   (interleave b (stream-cdr a)))))

(define (cartesian-product a b)
  (cons-stream (list (stream-car a) (stream-car b))
               (interleave (zip-over (stream-car a) (stream-cdr b))             
                           (cartesian-product (stream-cdr a) b))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define all-rationals
  (stream-map 
   (lambda (ab) (/ (car ab) (cadr ab)))
   (stream-filter (lambda (ab) 
                    (= 1 (gcd (car ab) (cadr ab))))
                  (cartesian-product integers integers))))

(eval-stream all-rationals 10)

;;;;;;;;;;
; Task 4 ;
;;;;;;;;;;

; No. I will use two properties of the set R (real numbers): 
; it is complete (every sequence with a limit has a limit in R)
; it is dense (between every two unequal real numbers is another real number)
; suppose a list of all real numbers were given, say (real n) is the nth number on the list
; then consider the following (non-terminating )function
; define (r n a b s)
;   if a < (real n) < b:
;     if (= s 1):
;       (r (+ n 1) a (real n) 0)
;     else:
;       (r (+ n 1) (real n) b 1)
;   else:
;     (r (+ n 1) a b s)
;
; consider an execution of (r 0 0 1 0), which calls itself recursively once per call. hence write a list of all calls of r. note that a never equals to b.
; as n increases, a and b get arbitrarily close to each other; otherwise there is some r' such that a < r' < b, which is impossibe because R is dense.
; hence, this sequence defines a limit rr, the limit of a as n increases which is also the limit of b as n increases. rr is in R because R is complete. rr is never equal to a or b because we keep switching which of a or b to reduce through the parameter s.
; hence rr is not equal to (r n) for any n. hence the list is incomplete. contradiction.

;;;;;;;;;;
; Task 5 ;
;;;;;;;;;;

(define all-coordinates
  (stream-map (lambda (xyz) (apply cons xyz))
              (cartesian-product integers
                                 (cartesian-product integers integers))))
