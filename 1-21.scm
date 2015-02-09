(define (smallest-divisor n)
  (find-divisor n 2)
)

(define (next num)
  (if (= num 2) 
  	   3
  	  (+ num 2)
  )
)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))
  )
)

(define (divides? n test-divisor)
   (= (remainder n test-divisor) 0)
)