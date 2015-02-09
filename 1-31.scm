(define (product term a next b)
   (if (> a b)
   	   1
   	   (* (term a) (product term (next a) next b))
   )
)

(define (product-iter term a next b)
	(define (iter k result)
	  (if (> k b)
	      result
	      (iter (next k) (* (term k) result))
	  )
	)
	(iter a 1)
)

(define (factorial n)
   (product identity 1 nexti n)
)

(define (identity i)
  i
)

(define (nexti i)
  (+ i 1)
)

(define (pie n)
  (if (odd? n)
  	  (* 2 (product-iter term (exact->inexact 1) nexti (/ (+ 1 n) 2)))
  	  (* 2 (product-iter term (exact->inexact 1) nexti (/ n 2)) (/ (+ n 2) (+ n 1)))	
  )	
)

(define (term i)
  	(/ (square (* 2 i)) (* (- (* 2 i) 1) (+ (* 2 i) 1)))
)