(define (make-accumulator init)
  (lambda (amount) (begin (set! init (+ amount init)) init))
)