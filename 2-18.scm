(define (reverse-rec lists)
  (if (null? (cdr lists))
      lists
      (append (reverse-rec (cdr lists)) (cons (car lists) '()))
  )
)

(define (reverse-iter lists)
  (define (iter list result)
    (if (null? list)
        result
        (iter (cdr list) (cons (car list) result))
    )
  )
  (iter lists '())
)