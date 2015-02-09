(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items) (accumulate op init (cdr items)))
  )
)

(define (accumulate-n op init seqs)
  (define (select-car s)
    (map car s)
  )

  (define (select-cdr s)
    (map cdr s)
  )

  (if (null? (car seqs))
      '()
      (cons (accumulate op init (select-car seqs)) 
      		(accumulate-n op init (select-cdr seqs)))
  )
)

