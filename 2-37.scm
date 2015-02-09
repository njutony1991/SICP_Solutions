(define (accumulate op init items)
   (if (null? items)
       init
       (op (car items) (accumulate op init (cdr items)))
   )
)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
      	    (accumulate-n op init (map cdr seqs)))
  )
)

(define (dot-product v w)
	(accumulate + 0 (map * v w))  
)

(define (matrix-*-vector m v)
  	(map (lambda (line) (dot-product line v)) m)
)

(define (transpose mat)
    (accumulate-n cons '() mat)
)

(define (matrix-*-matrix m n)
  	(let ((cols (transpose n)))
  		 (map (lambda (line)
  		 			  (matrix-*-vector cols line)) m)
  	)
)