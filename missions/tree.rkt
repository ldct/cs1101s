;; tree: lists of length three, cadr is a number
;; binary-tree: left <= val < right

(define (make-tree left val right)
  (list left val right))

(define (head tree)
  (if (pair? tree)
      (cadr tree)
      tree))

(define (left tree)
  (car tree))

(define (right tree)
  (caddr tree))

(define (show-tree tree)
  (define (show level tree)
    (if (or (null? tree) (number? tree))
        (begin (show-n-spaces level)
               (display tree))
        (begin (show (+ level 1) (right tree)) (display "\n")
               (show-n-spaces level) (display (head tree)) (display "\n")
               (show (+ level 1) (left tree)))))
  (show 1 tree))

(define (contains? tree val)
  (cond ((number? tree) (= tree val))
        ((not (pair? tree)) #f)
        ((= val (head tree)) #t)
        ((< val (head tree)) (contains? (left tree) val))
        ((> val (head tree)) (contains? (right tree) val))))

(define (leaves tree)
  (cond ((number? tree) (list tree))
        ((null? tree) ())
        (else (append (leaves (left tree))
                      (list (head tree))
                      (leaves (right tree))))))

(define (all p l)
  (or (null? l)
      (and (p (car l))
           (all p (cdr l)))))

(define (binary-search-tree? tree)
  (and (tree? tree)
       (or (null? tree)
           (number? tree)
           (and (all (lambda (t) (<= t (head tree))) (leaves (left tree)))
                (all (lambda (t) (> t (head tree))) (leaves (right tree)))))))
      
  
(define (tree? tree)
  (or (number? tree)
      (null? tree)
      (and (= 3 (length tree))
           (number? (head tree))
           (tree? (left tree))
           (tree? (right tree)))))

(define tree1 '((1 3 5) 7 (() 9 11)))
(define leaves1 (leaves tree1))
(binary-search-tree? tree1)

(define (nth lst n)
  (if (= n 0)
      (car lst)
      (nth (cdr lst) (- n 1))))

(define (split-n lst n)    ;[0,n) [n,end)
  (define (split head tail i)
    (if (= i 0)
        (cons head tail)
        (split (append head (list (car tail)))
               (cdr tail)
               (- i 1))))
  (split '() lst n))

(define (leaves->balanced-tree lst)
  (let ((len (length lst)))
    (cond ((= len 0) lst)
          ((= len 1) (car lst))
          (else 
           (let ((s (split-n lst (quotient len 2))))
             (make-tree (leaves->balanced-tree (car s))
                        (cadr s)
                        (leaves->balanced-tree (cddr s))))))))

(define (balance-BST tree)
  (leaves->balanced-tree (leaves tree)))

(define (show-n-spaces n)
  (if (= n 0)
      (lambda (x) x)
      (begin (display "___")
             (show-n-spaces (- n 1)))))