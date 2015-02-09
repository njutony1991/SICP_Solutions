(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3))
)

(define (smooth-n f)
  body
)