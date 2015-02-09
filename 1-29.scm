(define (sum term a next b)
	(if (> a b)
		0
	    (+ (term a) (sum term (next a) next b))
	)
)

(define (cube n)
  	(* n n n)
)

(define (integral f a b dx)
  	(define (add-dx x)
  	 	(+ x dx)
  	)
  	(* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
)

(define (simpson f a b n)
  	(define (delta)
  		(/ (- b a) n)  
  	)
  	(define (next k)
  		(+ 1 k)
  	)
  	(define (y k)
  	  (f (+ a (* k (delta)))
      )
  	)
    (define (factor k)
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2)
      )
    )
    (define (term k)
      (* (factor k) (y k)) 
    )

    (* (/ (delta) 3)
  	   (sum term (exact->inexact 0) next n)
    )
)