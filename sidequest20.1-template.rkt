;
; CS1101S --- Programming Methodology
;
; Sidequest 20.1
;
; Note that written answers are commented out to allow us to run your
; code easily while grading your problem set.


(load "streams.rkt")

;;;;;;;;;;
; Task 1 ;
;;;;;;;;;;

(define (differences lst)
  (if (= (length lst) 1)
      '()
      (cons (- (cadr lst) (car lst))
            (differences (cdr lst)))))

(define (fold-stream stream init)
  (cons-stream init (fold-stream (stream-cdr stream) 
                                 (+ init (stream-car stream)))))

(define (regenerate-truncated-whisper truncated-whisper)
  (if (= (length truncated-whisper) 1)
      (stream-constant (car truncated-whisper))
      (fold-stream (regenerate-truncated-whisper (differences truncated-whisper))
                   (car truncated-whisper))))
      

;;; test
(eval-stream (regenerate-truncated-whisper '(9 6 -5 -18 -27)) 9)
;;; expected result
;;; (9 6 -5 -18 -27 -26 -9 30 97)

;;;;;;;;;;
; Task 2 ;
;;;;;;;;;;

(define (split-stream s1 s2)
  (if (or (stream-null? s2)
          (stream-car s2))
      (list s1 s2)
      (split-stream (stream-append s1 (list->stream (list #f)))
                    (stream-cdr s2))))

(define (stream-length s)
  (if (stream-null? s)
      0
      (+ 1 (stream-length (stream-cdr s)))))

(define (stream-last s)
  (if (stream-null? (stream-cdr s))
      (stream-car s)
      (stream-last (stream-cdr s))))

(define (interpolate-stream s)
  (if (<= (stream-length s) 2)
      s
      (cons-stream (stream-car s)
                   (interpolate-stream (cons-stream (+ (stream-car s)
                                                       (/ (- (stream-last s) (stream-car s))
                                                          (- (stream-length s) 1)))
                                                    (stream-cdr (stream-cdr s)))))))

(define (stream->list s)
  (if (stream-null? s)
      '()
      (cons (stream-car s)
            (stream->list (stream-cdr s)))))

(define (split s)
  (define sp (split-stream (list (stream-car s)) (stream-cdr s)))
  (if (stream-null? (cadr sp))
      (cons (stream-filter values (car sp))
            (cdr sp))
      (list (interpolate-stream (stream-append (car sp)
                                               (list->stream (list (stream-car (cadr sp))))))
             (cadr sp))))

(define (regenerate known-whisper unknown-whisper)
  (cons-stream
   (if (null? known-whisper)
       (stream-car unknown-whisper)
       (car known-whisper))
   (stream-cdr
    (if (stream-null? unknown-whisper)
        (regenerate-truncated-whisper (stream->list known-whisper))
        (let ((s (split unknown-whisper)))
          (regenerate (stream-append known-whisper (if (or (stream-null? (car s)) (null? known-whisper)) 
                                                       (car s) 
                                                       (stream-cdr (car s))))
                      (cadr s)))))))

(define (regenerate-corrupted-whisper corrupted-whisper) 
  (regenerate '() corrupted-whisper))


;;; test
(eval-stream (regenerate-corrupted-whisper '(4 #f 6 #f #f #f -2 #f -2.5 #f #f)) 15)
;;; expected result
;;; (4 5 6 4 2 0 -2 -2.25 -2.5 -18.5 -86.5 -249.0 -495.5 -626.5 -1.5)

;;;;;;;;;;
; Task 3 ;
;;;;;;;;;;

(define (tofun s)
  (define (dispatch x)
    (if (<= x 0)
        (stream-car s)
        ((tofun (stream-cdr s))
         (- x 1))))
  dispatch)

(define (f-cdr f)
  (lambda (x)
    (f (+ x 1))))

(define (tostream f)
  (cons-stream (f 0)
               (tostream (f-cdr f))))

(define (int x)
  (inexact->exact (round x)))

(define (sum f a b)
  (define m (* 0.5 (+ a b)))
  (if (< (- b a) 0.00001)
      (* (- b a) (f (int m)))
      (+ (sum f a m)
         (sum f m b))))

(define (smooth f r)
  (set! r (* r 1.0))
  (define (dispatch x)
    (set! x (* x 1.0))
    (/ (sum f (- x r) (+ x r))
       (* 2.0 r)))
  dispatch)

(define (smooth-regenerated-whisper s r i)
  (if (= i 0) s
      (smooth-regenerated-whisper (tostream (smooth (tofun s) r)) r (- i 1))))

;;; test
(eval-stream (smooth-regenerated-whisper (regenerate-truncated-whisper '(4 7 2 5 8)) 1 1) 10)
;;; expected result
;;; (4.75 5.0 4.0 5.0 0.0 -43.0 -180.0 -491.0 -1080.0 -2075.0)
(eval-stream (smooth-regenerated-whisper (regenerate-truncated-whisper '(4 7 2 5 8)) 1 2) 10)
;;; expected result
;;; (4.8125 4.6875 4.5 3.5 -9.5 -66.5 -223.5 -560.5 -1181.5 -2214.5)
(eval-stream (smooth-regenerated-whisper (regenerate-truncated-whisper '(4 7 2 5 8)) 1.5 2) 4)
;;; expected result
;;; (4.777777777777778 4.666666666666667 4.666666666666667 2.3333333333333335)