(define (fast-expt b n)
   	   (fast-iter 1 b n)
)

(define (fast-iter a b n)
   (cond ((= n 0) a)
   	       ((even? n) (fast-iter a (* b b) (/ n 2)))
   	       ((odd? n) (fast-iter (* a b) b (- n 1)))
   ) 
)

