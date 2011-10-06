(define (empty-set) (lambda (t) #f))
 
(define (insert set value)
  (if (set value) 
      set
      (lambda (t) (or (= t value) (set t)))))
 
(define (delete set value)
  (if (set value) 
      (lambda (t) (and (not (= t value)) 
                       (set value)))
      set))
 
(define (union set1 set2)
  (lambda (t) 
    (or (set1 t) 
        (set2 t))))
 
(define (intersect set1 set2)
  (lambda (t) 
    (and (set1 t) 
         (set2 t))))

(define (complement set)
  (lambda (t) (not (set t))))

(define (cartesian-product-2 set1 set2)
  (lambda (t)
    (and (set1 (car t))
         (set2 (cdr t)))))
 
(define (print-setf set start next end)
  (if (<= start end)
      (begin 
        (if (set start) (begin (display start) (newline)))
        (print-setf set (next start) next end))))