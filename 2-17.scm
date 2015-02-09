(define (list-refe list n)
  (if (= n 0)
      (car list)
      (list-refe (cdr list) (- n 1))
  )
)

(define (length list)
  (define (iter items count)
    (if (null? items)
        count
        (iter (cdr items) (+ count 1))
    )
  )
  (iter list 0)
)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))
  )
)

(define (last-pair items)
  (if (null? (cdr items))
      items
  	  (last-pair (cdr items))    
  )
)

(define (last-pair-iter items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) items)
    )
  )
  (iter items items)
)