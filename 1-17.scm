(define (fast-* a b)
   (cond ((= b 0) 0)
         ((even? b) (double (fast-* a (halve b))))
         ((odd? b) (+ a (fast-* a (- b 1))))
   ) 
)

(define (halve n)
	(/ n 2)
)

(define (double n)
  	(* n 2)
)