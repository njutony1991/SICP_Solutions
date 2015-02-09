(define (fast-* a b)
   (fast-*-iter a b 0)
   	
)

(define (fast-*-iter a b c)
   (cond ((= b 0) c)
         ((even? b) (fast-*-iter (double a) (halve b) c))
         ((odd? b) (fast-*-iter (double a) (halve (- b 1)) (+ c a)))
   )
)

(define (halve n)
	(/ n 2)
)

(define (double n)
  	(* n 2)
)
