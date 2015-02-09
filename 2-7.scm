(define (make-interval a b)
  (cons a b)
)

(define (upper-bound inter)
  (max (cdr inter) (car inter))
)

(define (lower-bound inter)
  (min (cdr inter) (car inter))
)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
  		(p2 (* (lower-bound x) (upper-bound y)))
  		(p3 (* (upper-bound x) (lower-bound y)))
  		(p4 (* (upper-bound x) (upper-bound y)))
  	   )
  	(make-interval (min p1 p2 p3 p4)
  				   (max p1 p2 p3 p4)
  	)
  )
)