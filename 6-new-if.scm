(define (newif preidcate then-clause else-clause)
	((cond (preidcate then-clause)
	       (else else-clause)))
)