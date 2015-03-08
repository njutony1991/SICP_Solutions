(load "2-78.scm")
(define op-table* (make-equal-hash-table))

(define (put op type proc)
  (hash-table/put! op-table* (list op type) proc))

(define (get op type)
  (hash-table/get op-table* (list op type) '()))


(define coercion-table (make-equal-hash-table))

(define (put-coercion type1 type2 proc)
  (hash-table/put! coercion-table (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-table/get coercion-table (list type1 type2) '()))

(define (install-coercion-package)
   (define (scheme-number->rational n)
     (make-rational (contents n) 1))

   (put-coercion 'scheme-number 'rational scheme-number->rational)
   'done
)

(define (install-schemer-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))

  (put 'zero=? '(scheme-number) (lambda (x) (begin (display "in zero=? scheme-number")
                                            (= x 0))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'equ? '(scheme-number scheme-number) =)

  (put 'negate '(scheme-number) (lambda (x) (tag (- n))))
  'done
)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

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

  (put 'zero=? '(rational) (lambda (x) (begin (display "in zero=? rational")
                                              (= (numer x) 0))))

  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  (put 'negate '(rational) (lambda (rat) (make-rational (- (numer rat)) (denom rat))))

  'done
)

(define (make-rational n d)
  ((get 'make 'rational) n d)
)

(define (equ? x y)
   (apply-generic 'equ? x y)
)

(define (zero=? x)
  (apply-generic 'zero=? x))

(define (add x y)
  (apply-generic 'add x y)
)

(define (sub x y)
  (apply-generic 'sub x y))

(define (mul x y)
  (apply-generic 'mul x y))

(define (div x y)
  (apply-generic 'div x y))

(define (negate x) (apply-generic 'negate x))

(define (apply-generic op . args)
  (define (true-map proc sequence)
    (define (true-map-iter proc sequence result)
       (if (null? sequence)
           (reverse result)
           (let ((item (proc (car sequence))))
             (if item
                (true-map-iter proc (cdr sequence) (cons item result))
                #f
             )
           )
       )
    )
    (true-map-iter proc sequence '())
  )

  (define (iter type-tags args)
    (if (null? type-tags)
        (error? "No method for these types-ITER")
        (let ((type1 (car type-tags)))
          (let ((filtered-args (true-map (lambda (x) 
                                            (let ((type2 (type-tag x)))
                                              (if (eq? type1 type2)
                                                  x
                                                  (let ((t2->t1 (get-coercion type2 type1)))
                                                    (if (null? t2->t1)
                                                        #f
                                                        (t2->t1 x)))))) args)))
            (or filtered-args (iter (cdr type-tags) args))
          )
        )
    )
  )

  (let ((type-tags (map type-tag args)))
  	(let ((proc (get op type-tags)))
  		(if (not (null? proc))
  			(apply proc (map contents args))
  		  (apply apply-generic (cons op (iter type-tags args)))
  		)
  	)
  )
)

