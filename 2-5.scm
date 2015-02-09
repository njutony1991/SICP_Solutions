(define (cons x y)
  (* (expt 2 x)
  	 (expt 3 y)
  )
)

(define (car c)
  (if (not (= 0 (remainder c 2)))
      0
      (+ 1 (car (/ c 2)))
  )
)

(define (cdr c)
  (if (not (= 0 (remainder c 3)))
      0
      (+ 1 (cdr (/ c 3)))
  )
)