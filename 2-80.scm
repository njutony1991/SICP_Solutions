(load "2-79.scm")
(put '=zero? '(schemer-number) (lambda (x) (= x 0)))

(put '=zero? '(rational) (lambda (x) (= (numer x) 0)))

(put '=zero? '(complex) (lambda (x) (= (real-part x) (imag-part x) 0)))

(define (=zero? x)
  (apply-generic '=zero? x))