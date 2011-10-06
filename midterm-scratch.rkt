(define (permute lst)
  (if (null? lst)
      '(())
      (apply append (map (lambda (x) (map (lambda (y) (cons x y))
                                          (permute (filter (lambda (y) (not (eq? x y))) lst))))
                         lst))))

(permute '(1 2 3))

((lambda (x y z) (/ (+ x y) z)) 3 2 1)
(define (foo x)
  (if (odd? x)
      (- x 1)
      (+ x 2)))
(foo (foo (foo (foo (foo -4)))))
(define (ping n)
  (if (< n 1)
      1
      (pong (/ n 2))))
(define (pong n)
  (if (< n 1000)
      (ping (- n 1))
      33))
(ping 32)
(define (a x y) (x + y))
(define (b x y) (y - x))
((a b (lambda (x y) x)) 3 1)
(define (bar x) (x 1 4))
(let ((a bar)
      (bar (lambda (x y) 3)))
  (a bar))

(define (make-game-state p n)
  (if (= 0 p)
      '()
      (cons n (make-game-state (- p 1) n))))

(define (size-of-pile game-state p)
  (if (= p 0)
      (car game-state)
      (size-of-pile (cdr game-state) (- p 1))))

(define (remove-coins-from-pile game-state n p)
  (if (= p 0)
      (cons (- (car game-state) n) (cdr game-state))
      (cons (car game-state) (remove-coins-from-pile (cdr game-state)
                                                     n
                                                     (- p 1)))))
(define (over? game-state)
  (cond ((null? game-state)
         #t)
        ((not (= (car game-state) 0))
         #f)
        (else (over? (cdr game-state)))))

(define (moves-valid? game-state moves)
  (if (null? moves)
      (over? game-state)
      (let* ((move (car moves))
             (p (car move))
             (n (cadr move)))
        (if (< (size-of-pile game-state p) n)
            #f
            (moves-valid? (remove-coins-from-pile game-state n p)
                          (cdr moves))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (wtg n m a b)
  (if (and (= a 0) (= b 0)) 1
      (+ (cond ((= a 0) 0)
               (else (* n (wtg (- n 1) m (- a 1) b))))
         (cond ((= b 0) 0)
               (else (* m (wtg n (- m 1) a (- b 1))))))))

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (choose n r)
  (/ (fact n) (* (fact r) (fact (- n r)))))

(define (wtg-combi n m a b)
  (* (fact (+ a b))
     (choose n a)
     (choose m b)))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (wtg2-combi n m a b c)
  (foldr + 0 (map (lambda (x) (* (choose n x) (choose m (- c x))))
                (enumerate-interval a (- c b)))))

(define (ways-to-graduate2 n m a b c)
  (cond ((or (< c 0)
             (> a n)
             (> b m)) 0)
        ((and (= a 0)
              (= b 0)
              (= c 0)) 1)
        ((= m 0) (if (> a 0)
                     (+ (ways-to-graduate2 (- n 1) 0 a b c)
                        ; don’t take s n
                        (ways-to-graduate2 (- n 1) 0 (- a 1) b (- c 1)))
                     ; take s n
                     (+ (ways-to-graduate2 (- n 1) 0 a b c)
                        ; don’t take s n
                        (ways-to-graduate2 (- n 1) 0 a b (- c 1)))))
        ; take s n
        (else (if (> b 0)
                  (+ (ways-to-graduate2 n (- m 1) a b c)
                     ; don’t take h m
                     (ways-to-graduate2 n (- m 1) a (- b 1) (- c 1)))
                  ; take h m
                  (+ (ways-to-graduate2 n (- m 1) a b c)
                     ; don’t take h m
                     (ways-to-graduate2 n (- m 1) a b (- c 1)))))))
; take h m
