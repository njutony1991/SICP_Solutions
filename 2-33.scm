(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
      	  (accumulate op initial (cdr sequence)))
  )
)

(define (map-new p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence)
) 

(define (append-new seq1 seq2)
  (accumulate cons seq2 seq1)
)

(define (length-new sequence)
  (accumulate (lambda (x y) (if (null? x)
      							0
      							(+ 1 y))) 0 sequence)
)