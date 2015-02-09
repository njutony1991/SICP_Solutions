(define (fringe tree)
  (define (leaf trees result)
    (cond ((null? trees) result)
          ((not (pair? trees)) (cons trees result))
          (else (leaf (car trees)
          		  (leaf (cdr trees) result))
          )
    )
  )
  (leaf tree '())
)


(define (fringe-rec tree)
   (if (null? tree)
       '()
       (let ((first (car tree)))
       		(if (not (pair? first))
       	    	(cons first (fringe-rec (cdr tree)))
       	    	(append (fringe-rec first) (fringe-rec (cdr tree))) 
       		)
       )
   )
)