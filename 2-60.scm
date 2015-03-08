(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))
  )
)

(define (adjoin-set x set)
  (cons x set)
)

(define (union-set set1 set2)
  (append set1 set2)
)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) 
         	   (remove-set-element (car set1) (cdr set2)))))
        (else (intersection-set (cdr set1) set2))
  )
)

(define (remove-set-element x set)
  (define (remove-set-element-iter pre succ)
    (cond ((null? succ) pre)
          ((equal?  x (car succ)) (append pre (cdr succ)))
          (else (remove-set-element-iter (cons (car succ) pre) 
          		(cdr succ)))
    )
  )
  (remove-set-element-iter '() set)
)