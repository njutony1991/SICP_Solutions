(define (same-parity . p)
	(define (iter p head result)
	  (if (null? p)
	  	  result
	  	  (cond ((= (remainder (car p) 2) (remainder head 2)) 
	  	  		 (iter (cdr p) head (cons (car p) result)))
	  	        (else (iter (cdr p) head result))
	  	  )
	  )
	)
	(reverse (iter p (car p) '()))
)

(define (same-parity-improve first . rest)
  (let ((yes? (if (even? first)
  				   even? 
  				   odd? ))
	   )
  	(define (iter items result)
  	  (if (null? items)
  	      (reverse result)
  	      (iter (cdr items) (if (yes? (car items))
  	      					 	(cons (car items) result)
  	      					 	result)
  	  	  )
  	  )
  	)
  	(iter rest (list first))
  )
)