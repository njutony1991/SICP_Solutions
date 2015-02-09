(load "1-37.scm")
(define (tan-cf x k)
   (define (N i)
      (if (= i 1)
          x
          (- 0 (* x x))
      )
   )

   (define (D i)
     (- (* 2 i) 1)
   )

   (exact->inexact (cont-frac-iter N D k))
)