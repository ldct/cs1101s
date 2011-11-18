;******************************************************
;*
;*  CS1101S Practical Exam
;*  AY2011/2012, Semester 2
;*  Matric No: A0091997
;*
;******************************************************

;;;
;;; Question 1
;;;

(define (uniq lst)
  (define (helper u l)
    (if (null? l)
        u
        (if (member (car l) u)
            (helper u (cdr l))
            (helper (cons (car l) u) (cdr l)))))
  (reverse (helper '() lst)))

(define (count-list lst)
  (uniq (map (lambda (x)
               (list x (foldr + 0 (map (lambda (y) (if (eq? x y) 1 0)) lst))))
             lst)))
  
; Tests
(count-list '(a b c))
;((a 1) (b 1) (c 1))

(count-list '(d a a b a b c))
;((d 1) (a 3) (b 2) (c 1))

(count-list '(c))
;((c 1))

(count-list '())
;()

(count-list '(a a a a a a a a a a a a))
;((a 12)) 

;;;
;;; Question 2
;;;

(define (build-pyramid lst)
  (if (< (length lst) 3)
      lst
      (list (car lst)
            (build-pyramid (reverse (cdr (reverse (cdr lst)))))
            (car (reverse lst)))))
         

; Tests
(build-pyramid '(1))
;(1)

(build-pyramid '(1 2))
;(1 2)

(build-pyramid '(1 2 3 4 5))
;(1 (2 (3) 4) 5)

(build-pyramid '(1 2 3 4 5 6))
;(1 (2 (3 4) 5) 6)

(build-pyramid '(I cannot believe I have to do this))
;(I (cannot (believe (I have) to) do) this)

;;;
;;; Question 3
;;;

;;; Your answer here.

(define (copy! lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (copy! (cdr lst)))))

(define e-of car)
(define c-of cadr)

(define (make-polynomial loec)
  (lambda (op . args)
    (cond ((eq? op 'compute)
           (foldr + 0 (map (lambda (ec)
                             (* (c-of ec) (expt (car args) (e-of ec))))
                           loec)))
          ((eq? op 'repr)
           loec)
          ((eq? op 'add)
           (make-polynomial (append (copy! loec)
                                    (copy! ((car args) 'repr)))))
          ((eq? op 'differentiate)
           (set! loec (filter (lambda (x) (not (eq? x 'delete-me)))
                              (map (lambda (ec)
                                     (if (= (car ec) 0)
                                         'delete-me
                                         (list (- (e-of ec) 1)
                                               (* (e-of ec) (c-of ec)))))
                                   loec))))
          (else (display (cons "no operation" op))))))

; Tests
(define f (make-polynomial '((1 1)))) ; f(x) = x
(f 'compute 5) ; f(5) = 5
(f 'compute 2) ; f(2) = 2
(f 'compute 4) ; f(4) = 4

(define g (make-polynomial '((1 1) (2 -2) (3 1) (0 -2)))) ; g(x) = x^3 - 2x^2 + x - 2
(g 'compute -1) ; g(-1) = -6
(g 'compute 0)  ; g(0) = -2
(g 'compute 1)  ; g(1) = -2

(define h (make-polynomial '((1 1) (2 -1) (3 1) (0 -2) (2 -1))))  ; h(x) = x^3 - 2x^2 + x - 2 = g(x) 
(h 'compute -1) ; h(-1) = -6
(h 'compute 0)  ; h(0) = -2
(h 'compute 1)  ; h(1) = -2

(define p (g 'add g)) ; p(x) = 2g(x) =  2x^3 - 4x^2 + 2x - 4
(p 'compute -1) ; p(-1) = -12
(p 'compute 0)  ; p(0) = -4
(p 'compute 1)  ; p(1) = -4

(p 'differentiate) ; p'(x) = 6x^2 - 8x + 2

(p 'compute -1) ; p'(-1) = 16
(p 'compute 0) ; p'(0) = 2
(p 'compute 1) ; p'(0) = 0

; Check that g didn't get modified 
(g 'compute -1) ; g(-1) = -6
(g 'compute 0)  ; g(0) = -2
(g 'compute 1)  ; g(1) = -2


;;;
;;; Question 4
;;;

(define eps 1/100)

(define (constant p)
  (p eps))

(define (limit-to-zero f)
  (define (helper int)
    (if (< (abs
            (- 1 (/ (- (f int)) (f (/ int 2))))) eps)
        (f int)
        (helper (/ int 2))))
  (helper eps))

(define (rest-of p)
  (lambda (x)
    (/ (- (p x) (limit-to-zero p))
       x)))

; Tests
(define f (lambda (x) (* 5 (expt x 1))))

;(find-polynomial f)
;;(0 0 0 5)
;
;(define g (lambda (x) (+  (* 6 (expt x 6))
;                           (* -2 (expt x 5))
;                           (* 3 x) 
;                           (* 5 (expt x 3)))))
;(find-polynomial g)
;;(0 3 0 5 0 -2 6)
;
;(define h (lambda (x) (+  5
;                           (* 3 (expt x 2)) 
;                           (* -2 (expt x 11))
;                           (* 1 x) 
;                           (* 3 (expt x 15)))))
;(find-polynomial h)
;(5 1 3 0 0 0 0 0 0 0 0 -2 0 0 0 3)


;;=======================================================================================
;; Grading of Practical Exam
;;=======================================================================================

(define (test-ans ans sol)
  (if (equal? ans sol)
      'ok
      (begin (display (list "Ans=" ans))
             (newline)
             (display (list "Sol=" sol))
             (newline))))

'Q1
'===
(test-ans (count-list '(a b c)) '((a 1) (b 1) (c 1)))
(test-ans (count-list '(1 2 3 4 5 1 1 3 4 5)) '((1 3) (2 1) (3 2) (4 2) (5 2)))
(test-ans (count-list '(d a a b a b c)) '((d 1) (a 3) (b 2) (c 1)))
(test-ans (count-list '(c)) '((c 1)))
(test-ans (count-list '()) '())
(test-ans (count-list '(() () ())) '((() 3)))
(test-ans (count-list '((()) () ())) '(((()) 1) (() 2)))
(test-ans (count-list '(a a a a a a a a a a a a)) '((a 12))) 
(test-ans (count-list '((a b) a b)) '(((a b) 1) (a 1) (b 1)))
(test-ans (count-list '((a b) (a b))) '(((a b) 2)))
(test-ans (count-list '((a (b) c) (a b))) '(((a (b) c) 1) ((a b) 1)))
(test-ans (count-list '(a (a b) b)) '((a 1) ((a b) 1) (b 1)))

'Q2
'===
(test-ans (build-pyramid '(1)) '(1))
(test-ans (build-pyramid '(1 2)) '(1 2))
(test-ans (build-pyramid '(1 2 3)) '(1 (2) 3))
(test-ans (build-pyramid '(1 2 3 4)) '(1 (2 3) 4))
(test-ans (build-pyramid '(1 2 3 4 5)) '(1 (2 (3) 4) 5))
(test-ans (build-pyramid '(1 2 3 4 5 6)) '(1 (2 (3 4) 5) 6))
(test-ans (build-pyramid '(I cannot believe I have to do this)) '(I (cannot (believe (I have) to) do) this))
(test-ans (build-pyramid '()) '())
(test-ans (build-pyramid '(1 (2 3 4 5) 6)) '(1 ((2 3 4 5)) 6))
(test-ans (build-pyramid '((1) (2) 3 4 (5) 6)) '((1) ((2) (3 4) (5)) 6))
(test-ans (build-pyramid '(a (b (c (d))))) '(a (b (c (d)))))
(test-ans (build-pyramid '(a (b) c)) '(a ((b)) c))

'Q3
'===
(define f (make-polynomial '((1 1)))) ; f(x) = x
(test-ans (f 'compute 5) 5)
(test-ans (f 'compute 2) 2)
(test-ans (f 'compute 4) 4)

(define g (make-polynomial '((1 1) (2 -2) (3 1) (0 -2)))) ; g(x) = x^3 - 2x^2 + x - 2
(test-ans (g 'compute -1) -6)
(test-ans (g 'compute 0) -2)
(test-ans (g 'compute 1) -2)

(define h (make-polynomial '((1 1) (2 -1) (3 1) (0 -2) (2 -1))))  ; h(x) = x^3 - 2x^2 + x - 2 = g(x) 
(test-ans (h 'compute -1) -6)
(test-ans (h 'compute 0) -2)
(test-ans (h 'compute 1) -2)

(define p (g 'add g)) ; p(x) = 2g(x) =  2x^3 - 4x^2 + 2x - 4
(test-ans (p 'compute -1) -12)
(test-ans (p 'compute 0) -4)
(test-ans (p 'compute 1) -4)

(p 'differentiate) ; p'(x) = 6x^2 - 8x + 2
(test-ans (p 'compute -1) 16)
(test-ans (p 'compute 0) 2)
(test-ans (p 'compute 1) 0)

; Check that g didn't get modified 
(test-ans (g 'compute -1) -6)
(test-ans (g 'compute 0) -2)
(test-ans (g 'compute 1) -2)

(define q (p 'add p))
(test-ans (q 'compute 0) 4)
(test-ans (q 'compute 100) 118404)
(test-ans (q 'compute 74) 64532)

(define r (make-polynomial '()))
(test-ans (r 'compute 0) 0)
(define s (r 'add p))
(test-ans (r 'compute 0) 0)
(test-ans (r 'compute 1) 0)
(test-ans (r 'compute -1) 0)

(test-ans (s 'compute 0) 2)
(test-ans (s 'compute 1) 0)
(test-ans (s 'compute -1) 16)


'Q4
'===
(define f (lambda (x) (* 5 (expt x 3))))

(test-ans (find-polynomial f) '(0 0 0 5))

(define g (lambda (x) (+  (* 6 (expt x 6))
                          (* -2 (expt x 5))
                          (* 3 x) 
                          (* 5 (expt x 3)))))
(test-ans (find-polynomial g) '(0 3 0 5 0 -2 6))

(define p (lambda (x) (+  5
                          (* 3 (expt x 2))
                          (* -2000 (expt x 4))
                          (* 1 x) 
                          (* 3 (expt x 5)))))

(test-ans (find-polynomial p) '(5 1 3 0 -2000 3))

(define q (lambda (x) 0))
(test-ans (find-polynomial q) '())

(define r (lambda (x) (* 3 (expt x 2))))

(test-ans (find-polynomial r) '(0 0 3))

(define h (lambda (x) (+  5
                          (* 3 (expt x 2))
                          (* -2 (expt x 11))
                          (* 1 x) 
                          (* 3 (expt x 15)))))
(test-ans (find-polynomial h) '(5 1 3 0 0 0 0 0 0 0 0 -2 0 0 0 3))



;Results:
;Q1:   9/10   - Minor bug
;Q2:  10/10 
;Q3:  10/10  
;Q4:   2/20   - for effort
;--------------------
;Total: 29/30
;Bonus: 2
