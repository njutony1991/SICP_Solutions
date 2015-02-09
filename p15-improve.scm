(define (average x y)
 	(/ (+ x y) 2))
(define (improve guess x)
  	(average guess (/ x guess)))

(define (improve2 guess x)
  	(/ (+ (* 2 guess) (/ x (* guess guess))) 3))