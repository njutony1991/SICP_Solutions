(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))
  )
)

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
      		(x2 (car set2)))
      	(cond ((= x1 x2)
      		   (cons x1 (intersection-set (cdr set1)
      		   							  (cdr set2))))
      	      ((< x1 x2)
      	       (intersection-set (cdr set1) set2))
      	      ((< x2 x1)
      	       (intersection-set set1 (cdr set2)))
      	)
      )
  )
)

(define (adjoin-set x set)
  (define (adjoin-set-iter pre succ)
  	(cond ((null? succ) (append pre (list x)))
  		  ((= x (car succ)) (append pre succ))
  	      ((> x (car succ)) (adjoin-set-iter (append pre (list (car succ))) 
  	      									 (cdr succ)))
  	      ((< x (car succ)) (append pre (cons x succ)))
  	)
  )
  (adjoin-set-iter '() set)
)

(define (adjoin-set-new x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-new x (cdr set))))
  )
)