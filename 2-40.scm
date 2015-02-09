(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))
  )
)

(define (filter predicate seq)
  (cond ((null? seq) '())
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))
  )
)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))
  )
)

(define (flat_map proc seq)
   (accumulate append '() (map proc seq))
)

(define (prime-sum? pair)
   (prime? (+ (car pair) (cadr pair)))
)

(define (prime? num)
  (define (test divisor)
    (cond ((> (* divisor divisor) num) #t)
          ((= 0 (remainder num divisor)) #f)
          (else (test (+ divisor 1)))
    )
  )
  (test 2)
)

(define (make-prime-sum pair)
   (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (unique-pairs n)
   (flat_map (lambda (i) (map (lambda (j) (list i j)) 
   							 (enumerate-interval 1 (- i 1)))) 
			(enumerate-interval 1 n))
)

(define (prime-sum-pairs n)
  (map make-prime-sum (filter prime-sum? 
  					  (unique-pairs n)))
)