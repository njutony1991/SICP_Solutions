(define op-table* (make-strong-eq-hash-table))

(define (put op type proc)
  (hash-table/put! op-table* (list op type) proc))

(define (get op type)
  (hash-table/get op-table* (list op table) '()))

(define (install-sum-package)
   (define (sum-deriv expr var)
      (make-sum (deriv (addend expr) var)
      			(deriv (augend expr) var)))
   (define (addend expr)
     (car expr))

   (define (augend expr)
     (cadr expr))

   (define (number=? e n)
     (and (number? e) (= e n))
   )

   (define (make-sum exp1 exp2)
     (cond ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
           ((number=? exp1 0) exp2)
           ((number=? exp2 0) exp1)
           (else (list '+ exp1 exp2))
     )
   )

   (define (mul-deriv expr var)
     (make-sum (make-product (multiplier expr) (deriv (multiplicand expr) var))
     		   (make-product (deriv (multiplier expr) var) (multiplicand expr)))
   )

   (define (multiplier expr)
     (car expr))
   (define (multiplicand expr)
     (cadr expr))

   (define (make-product expr1 expr2)
     (cond ((and (number? expr1) (number? exp2)) (* expr1 expr2))
           ((number?= expr1 1) expr2)
           ((number?= expr2 1) expr1)
           ((or (number?= expr1 0) (number?= expr2 0)) 0)
           (else (list '* expr1 expr2))
     )
   )

   (put 'deriv '+ sum-deriv)
   (put 'deriv '* mul-deriv)
)


(define (exponent e)
   (cadr e))

(define (base e)
   (car e))

(define (make-exponent base exp)
  (cond ((number=? base 1) 1)
        ((number=? exp 0) 1)
        ((number=? base 0) 0)
        ((number=? exp 1) base)
    	(else (list '** base exp)))
)

(define (exponent-deriv expr var)
  (make-product (exponent expr) (make-product 
  				(make-exponent (base expr) (make-sum (exponent expr) -1))
  				(deriv (base expr) var)))
)

(put 'deriv '** exponent-deriv)