(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
        	(if (same-variable? exp var)
        	    1
        	    0))
        ((sum? exp)
        	(make-sum (deriv (addend exp) var)
        			  (deriv (augend exp) var)))
        ((product? exp)
        	(make-sum (make-product (multiplier exp)
        							(deriv (multiplicand exp) var)) 
        			  (make-product (deriv (multiplier exp) var)
        			  				(multiplicand exp))))
        ((exponentiation? exp)
          (make-product
                (make-product (exponent exp)
                      (make-exponentiation (base exp) 
                            (make-sum (exponent exp) -1)))
              (deriv (base exp) var)))
        (else 
        	(error "unknown expression type -- DERIV" exp))
  )
)

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))
  )
)

(define (variable? e)
  (symbol? e)
)

(define (same-variable? e1 e2)
  (and (variable? e1) (variable? e2) (eq? e1 e2))
)

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend e)
  (cadr e))

(define (augend e)
  (accumulate make-sum 0 (cddr e)))

(define (number=?  exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((number=? a1 0) a2)
        ((number=? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))
  )
)

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier e)
  (cadr e))

(define (multiplicand e)
  (accumulate make-product 1 (cddr e)))

(define (make-product m1 m2)
  (cond ((or (number=? m1 0) (number=? m2 0)) 0)
        ((number=? m1 1) m2)
        ((number=? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
  )
)

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)) 
)

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation b e)
  (cond ((number=? b 1) 1)
        ((number=? b 0) 0)
        ((number=? e 0) 1)
        ((number=? e 1) b)
        (else (list '** b e))
  )
)