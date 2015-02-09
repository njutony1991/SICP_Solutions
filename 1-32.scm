(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a) (accumulate combiner null-value term (next a) next b))
	)	     
)

(define (accumulate-iter combiner null-value term a next b)
	(define (iter k result)
	 	( if(> k b)
	 		result
	 		(iter (next k) (combiner (term k) result))
	 	)
	)
	(iter a null-value)
)


(define (sum term a next b)
	(define (add a b)
   		(+ a b)
	)
	(accumulate add 0 term a next b)
)

(define (product term a next b)
  	(define (mul a b)
    	(* a b)
	)
	(accumulate-iter mul 1 term a next b)
)

(define (identity n)
  	n
)

(define (next n)
  	(+ n 1)
)