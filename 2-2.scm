(define (make-segment point1 point2)
  (cons point1 point2)
)

(define (start-segment segment)
  (car segment)
)

(define (end-segment segment)
  (cdr segment)
)

(define (midpoint-segment segment)
  (let ((lpoint (start-segment segment))
        (rpoint (end-segment segment)))
  	(make-point (/ (+ (x-point lpoint) (x-point rpoint)) 2.0) 
  	            (/ (+ (y-point lpoint) (y-point rpoint)) 2.0))
  )
)

(define (make-point x y)
  (cons x y)
)

(define (x-point point)
  (car point)
)

(define (y-point point)
  (cdr point)
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)