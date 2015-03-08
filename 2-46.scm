(define (make-vect x y)
  (cons x y)
)

(define (xcor-vect vect)
  (car vect)
)

(define (ycor-vect vect)
  (cdr vect)
)

(define (add-vect v1 v2)
  (cons (+ (car v1) (car v2)) (+ (cdr v1) (cdr v2)))
)

(define (sub-vect v1 v2)
  (cons (- (car v1) (car v2)) (- (cdr v1) (cdr v2)))
)

(define (scale-vect vec factor)
  (cons (* factor (car vec)) (* factor (cdr vec)))
)