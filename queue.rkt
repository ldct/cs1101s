(require racket/mpair)

(define (make-queue) (mcons '() '()))
(define (empty-queue? q) (null? (mcar q)))
(define (insert! q item)
  (if (empty-queue? q)
      (begin (set-mcar! q (mcons item '()))
             (set-mcdr! q (mcar q)))
      (begin (set-mcdr! (mcdr q) (mcons item '()))
             (set-mcdr! q (mcdr (mcdr q))))))
(define (delete! q)
  (set-mcar! q (mcdr (mcar q))))

; The following have been run already:
; (require racket/mpair)

; complete this procedur

(define (is-node? node)
  (and (mpair? node)
       (or (null? node)
           (eq? 'node (mcar node)))))


; Add checks to ensure that prev and next are either null or nodes
(define (make-node object prev next)
  (if (and (is-node? prev)
           (is-node? next))
      (mlist 'node object prev next)
      "Non-node input"))
  