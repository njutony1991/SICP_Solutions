(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001)
)

(define (fixed-point f firstguess)
  	(define (try guess)
  	  (let ((next (f guess))) 
        (display next)
        (newline)
  	  	(if (close-enough? guess next)
  	  		next
  	  		(try next)
  	  	)
  	  )
  	)
  	(try firstguess)
)


(define (cube n)
    (* n n n)
)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0)
)

(define (average a b)
  (/ (+ a b) 2)
)

(define goldenratio
  (fixed-point (lambda (x) (+ (/ 1 x) 1)) 1.0)
)