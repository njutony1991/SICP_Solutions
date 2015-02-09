(load "1-22.scm")

(define (filtered-accumlate combiner valid nullvalue term a next b)
	(define (iter k result)
	  (cond ((> k b) result)
	        ((valid (term k)) (iter (next k) (combiner (term k) result)))
	        (else (iter (next k) result))
	  )
	)
	(iter a nullvalue)
)

(define (identity n)
	n
)

(define (next i)
	(+ i 1)
)

(define (prime-sum from to)
	(define (add a b)
	  	(+ a b)
	)
    (filtered-accumlate add prime? 0 identity from next to)
)

(define (gcd a b)
  	(if (= b 0)
  	      a
  	    (gcd b (remainder a b))
  	)
)

(define (coprimes-product n)
  	(define (mul a b)
  		(* a b)
  	)
  	(define (coprime? a)
  		(= (gcd a n) 1)
	)
	(filtered-accumlate mul coprime? 1 identity 0 next (- n 1))
)