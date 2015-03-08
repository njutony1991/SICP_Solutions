(load "2-78.scm")

(define op-table* (make-equal-hash-table))

(define (put op type proc)
  (hash-table/put! op-table* (list op type) proc))

(define (get op type)
  (hash-table/get op-table* (list op type) '()))

(define (install-schemer-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'equ? '(scheme-number scheme-number) =)
  'done
)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x)
)

(define (install-complex-package)
  ;;...
  (define (equal? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))
  )
  (put 'equ? '(complex complex) equal?)
  'done
)

(define (install-rational-package)
  (define (numer x) (car x))

  (define (denom x) (cdr x))

  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (tag x)
    (attach-tag 'rational x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
  )

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
  )

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (equal? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x)))
  )

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational) equal?)

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done
)

(define (make-rational n d)
  ((get 'make 'rational) n d)
)
(define (equ? x y)
   (apply-generic 'equ? x y)
)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
  	(let ((proc (get op type-tags)))
  		(if (not (null? proc))
  		    (apply proc (map contents args))
  			(error 
  				"No method for these types -- APPLY-GENERIC"
  				(list op type-tags))    
  		)
  	)
  )
)

(define (add x y)
  (apply-generic 'add x y)
)

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))