(load "1-42.scm")
(define (repeated f n)
  (define (iter g i)
    (if (= i 1)
        g
    	(iter (lambda (x) (g (f x))) (- i 1))    
    )
  )
  (iter f n)
)

(define (repeated-re f n)
  (if (= n 1)
      f
      (lambda (x) (f ((repeated-re f (- n 1)) x))) 
  )
)