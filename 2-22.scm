(define (square-list-test items)
  (define (iter things result)
    (if (null? things)
        result
        (iter (cdr things) 
        	  (cons result
        	  		(square (car things))))
    )
  )
  (iter items '())
)