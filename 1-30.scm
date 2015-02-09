(define (sum term a next b)
   (define (iter a result)
      (if(> a b)
       	 result
      	 (iter (next a) (+ result (term a)))
  	  )
   )
   (iter a 0)
)

(define (cube n)
	(* n n n) 
)

(define (next i)
 	(+ i 1)
)