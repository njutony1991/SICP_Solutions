(load "1-37.scm")
(define (e-value k)
  (define (D i)
    (if (= 0 (remainder (+ i 1) 3))
        (* 2 (/ (+ i 1) 3))
        1
    )
  )
  (+ 2.0 (cont-frac-iter (lambda (i) 1) D k))
)